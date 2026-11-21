;;; gptel-ui-links-test.el --- Chat link regressions -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel-ui-links)

(ert-deftest gptel-ui-links-render-preserves-transcript ()
  (with-temp-buffer
    (buffer-enable-undo)
    (insert (propertize "See [the source](src/main.el:12:3) and [[https://example.com][docs]]."
                        'gptel 'response 'front-sticky '(gptel)))
    (set-buffer-modified-p nil)
    (let ((text (buffer-string)) (undo buffer-undo-list)
          (overlays (gptel-ui-links-render (point-min) (point-max))))
      (should (= 2 (length overlays)))
      (should (equal-including-properties text (buffer-string)))
      (should (equal undo buffer-undo-list))
      (should-not (buffer-modified-p))
      (should (equal (mapcar (lambda (ov) (overlay-get ov 'gptel-ui-link)) overlays)
                     '("src/main.el:12:3" "https://example.com")))
      (mapc #'delete-overlay overlays)
      (should (equal-including-properties text (buffer-string))))))

(ert-deftest gptel-ui-links-local-location-formats ()
  (dolist (target '("src/main.el:12:3" "src/main.el::12:3"
                    "file:src/main.el::12:3"))
    (let ((destination (gptel-ui-links--destination target)))
      (should (eq (plist-get destination :type) 'file))
      (should (equal (plist-get destination :path) "src/main.el"))
      (should (= (plist-get destination :line) 12))
      (should (= (plist-get destination :column) 3))))
  (dolist (target '("src/main.el#L12-L19" "src/main.el:12-19"))
    (let ((destination (gptel-ui-links--destination target)))
      (should (equal (plist-get destination :path) "src/main.el"))
      (should (= (plist-get destination :line) 12))))
  (should (equal (plist-get (gptel-ui-links--destination
                            "file:///tmp/hello%20world.el") :path)
                 "/tmp/hello world.el"))
  (should (equal (plist-get (gptel-ui-links--destination
                            "file://localhost/tmp/hello.el") :path)
                 "/tmp/hello.el")))

(ert-deftest gptel-ui-links-rejects-special-protocols ()
  (dolist (target '("javascript:alert(1)" "elisp:(message hello)" "shell:ls"
                    "file://another-host/tmp/source.el" "/ssh:host:/tmp/source.el"
                    "file:%2Fssh:host:%2Ftmp%2Fsource.el" "#heading"))
    (should-not (gptel-ui-links--destination target))))

(ert-deftest gptel-ui-links-balanced-markdown-and-streaming ()
  (with-temp-buffer
    (insert "[one](https://example.com/a_(b)) [two](<some file.el:9> \"Title\")\n"
            "[unfinished](https://example.com/unfinished\n")
    (let ((overlays (gptel-ui-links-render (point-min) (point-max))))
      (should (cl-find "https://example.com/a_(b)" overlays
                       :key (lambda (ov) (overlay-get ov 'gptel-ui-link))
                       :test #'equal))
      (should (cl-find "some file.el:9" overlays
                       :key (lambda (ov) (overlay-get ov 'gptel-ui-link))
                       :test #'equal))
      ;; A streamed unfinished link does not hide any of its Markdown syntax.
      (should-not (cl-find-if
                   (lambda (ov) (and (overlay-get ov 'display)
                                     (string-match-p "unfinished"
                                                     (overlay-get ov 'gptel-ui-link))))
                   overlays)))))

(ert-deftest gptel-ui-links-skips-excluded-regions ()
  (with-temp-buffer
    (insert "[code](source.el)\n[prose](source.el)\n")
    (let ((overlays (gptel-ui-links-render
                     (point-min) (point-max)
                     (lambda (begin _end) (< begin 18)))))
      (should (= 1 (length overlays)))
      (should (equal (overlay-get (car overlays) 'display) "prose")))))

(ert-deftest gptel-ui-links-resolves-when-activated ()
  (let ((directory (make-temp-file "gptel-chat-link-" t)))
    (unwind-protect
        (let ((file (expand-file-name "source.el" directory)) opened)
          (with-temp-file file (insert "first\nsecond\nthird\n"))
          (with-temp-buffer
            (insert "[source](source.el:2:3)")
            (let ((overlay (car (gptel-ui-links-render (point-min) (point-max)))))
              ;; Workspace assignment happens after restoration and rendering.
              (setq default-directory directory)
              (cl-letf (((symbol-function 'find-file-other-window)
                         (lambda (path)
                           (setq opened path)
                           (erase-buffer)
                           (insert-file-contents path))))
                (gptel-ui-links--visit
                 (overlay-get overlay 'gptel-ui-link))
                (should (equal opened file))
                (should (= (line-number-at-pos) 2))
                (should (= (current-column) 2))))))
      (delete-directory directory t))))

(ert-deftest gptel-ui-links-code-paths-must-exist ()
  (let ((directory (make-temp-file "gptel-chat-link-" t)))
    (unwind-protect
        (let ((file (expand-file-name "source.el" directory)))
          (with-temp-file file (insert "hello"))
          (with-temp-buffer
            (setq default-directory directory)
            (insert "`source.el:1` and `missing.el:1` and `ordinary code`.\n"
                    "=source.el:1= and ~source.el:1~")
            (let ((overlays (gptel-ui-links-render (point-min) (point-max))))
              (should (= 3 (length overlays)))
              (should (equal (overlay-get (car overlays) 'gptel-ui-link)
                             "source.el:1")))))
      (delete-directory directory t))))

(ert-deftest gptel-ui-links-bare-url-retains-balanced-delimiters ()
  (with-temp-buffer
    (insert "See https://example.com/a_(b), then (https://example.com/c).")
    (should (equal (mapcar (lambda (ov) (overlay-get ov 'gptel-ui-link))
                          (gptel-ui-links-render (point-min) (point-max)))
                   '("https://example.com/a_(b)" "https://example.com/c")))))

(ert-deftest gptel-ui-links-web-activation-and-missing-file ()
  (let (visited)
    (cl-letf (((symbol-function 'browse-url) (lambda (url &rest _) (setq visited url))))
      (gptel-ui-links--visit "https://example.com/source#L4")
      (should (equal visited "https://example.com/source#L4")))
    (should-error (gptel-ui-links--visit "/this/chat/path/does/not/exist.el")
                  :type 'user-error)))

(ert-deftest gptel-ui-links-keyboard-opens-sole-table-destination ()
  (with-temp-buffer
    (insert "| source | source |\n")
    (goto-char (point-min))
    (let ((overlay (make-overlay (point-min) (point-max))) opened)
      (overlay-put overlay 'gptel-ui-table t)
      (overlay-put overlay 'display
                   (concat (gptel-ui-links-string "source" "src/main.el:8")
                           " | "
                           (gptel-ui-links-string "same source" "src/main.el:8")))
      (cl-letf (((symbol-function 'gptel-ui-links--visit)
                 (lambda (target) (setq opened target)))
                ((symbol-function 'completing-read)
                 (lambda (&rest _) (ert-fail "Single destination should not prompt"))))
        (gptel-ui-links-open)
        (should (equal opened "src/main.el:8"))))))

(ert-deftest gptel-ui-links-keyboard-selects-labeled-table-destination ()
  (with-temp-buffer
    (insert "| one | two |\n")
    (goto-char (point-min))
    (let ((overlay (make-overlay (point-min) (point-max))) opened candidates)
      (overlay-put overlay 'gptel-ui-table t)
      (overlay-put overlay 'display
                   (concat (gptel-ui-links-string "Source" "src/main.el:8")
                           " | "
                           (gptel-ui-links-string "Documentation" "https://example.com")))
      (cl-letf (((symbol-function 'gptel-ui-links--visit)
                 (lambda (target) (setq opened target)))
                ((symbol-function 'completing-read)
                 (lambda (_prompt collection _predicate require-match &rest _)
                   (should require-match)
                   (setq candidates collection)
                   (caar (cdr collection)))))
        (gptel-ui-links-open)
        (should (= 2 (length candidates)))
        (should (string-match-p "Documentation.*https://example.com" (caadr candidates)))
        (should (equal opened "https://example.com"))))))

(ert-deftest gptel-ui-links-blank-table-click-does-not-open-link ()
  (with-temp-buffer
    (insert "| source |\n")
    (goto-char (point-min))
    (let ((overlay (make-overlay (point-min) (point-max))))
      (overlay-put overlay 'gptel-ui-table t)
      (overlay-put overlay 'display
                   (concat " | " (gptel-ui-links-string "source" "source.el")))
      (cl-letf (((symbol-function 'event-start) (lambda (_) 'position))
                ((symbol-function 'posn-string) (lambda (_) (cons " | " 1)))
                ((symbol-function 'mouse-set-point) #'ignore)
                ((symbol-function 'gptel-ui-links--visit)
                 (lambda (_) (ert-fail "Blank table clicks must not open links"))))
        (should-error (gptel-ui-links-open 'mouse-event) :type 'user-error)))))

(ert-deftest gptel-ui-links-table-without-links-reports-no-target ()
  (with-temp-buffer
    (insert "| ordinary text |\n")
    (goto-char (point-min))
    (let ((overlay (make-overlay (point-min) (point-max))))
      (overlay-put overlay 'gptel-ui-table t)
      (overlay-put overlay 'display "ordinary text")
      (should-error (gptel-ui-links-open) :type 'user-error))))

(ert-deftest gptel-ui-links-overlap-index-obeys-half-open-boundaries ()
  ;; Compare every nonempty query with the simple definition, including
  ;; adjacency, containment, partial overlap, and spans across several ranges.
  (dolist (ranges '([] [(2 . 4)] [(2 . 4) (6 . 8) (8 . 10)]))
    (dotimes (begin 12)
      (cl-loop for end from (1+ begin) to 13 do
               (should
                (eq (and (gptel-ui-links--overlap-p begin end ranges) t)
                    (and (cl-some (lambda (range)
                                    (and (< begin (cdr range))
                                         (> end (car range))))
                                  ranges)
                         t)))))))

(ert-deftest gptel-ui-links-precedence-excludes-nested-and-partial-overlap ()
  (with-temp-buffer
    (insert "[outer](https://markdown.test/[[https://org.test]])\n"
            "[[https://adjacent.test]][md](target.el)\n"
            "https://partial.test/[md](target.el)\n"
            "`[code](target.el)`\n")
    ;; Make the code-quoted candidate eligible too: its Markdown link wins
    ;; because Markdown is a higher-precedence grammar pass.
    (cl-letf (((symbol-function 'gptel-ui-links--resolve) #'identity)
              ((symbol-function 'file-exists-p) (lambda (_) t)))
      (let ((overlays (gptel-ui-links-render (point-min) (point-max))))
        (should
         (equal (mapcar (lambda (ov)
                          (buffer-substring-no-properties
                           (overlay-start ov) (overlay-end ov)))
                        overlays)
                '("[[https://org.test]]"
                  "[[https://adjacent.test]]" "[md](target.el)"
                  "[md](target.el)" "[code](target.el)")))
        ;; Touching links coexist; a lower-precedence candidate that extends
        ;; beyond an existing link is excluded in its entirety.
        (should (= (overlay-end (nth 1 overlays))
                   (overlay-start (nth 2 overlays))))))))

(ert-deftest gptel-ui-links-rejected-candidates-do-not-reserve-ranges ()
  (with-temp-buffer
    (insert "[https://markdown-fallback.test ](javascript:bad)\n"
            "[[javascript:bad][https://org-fallback.test]]\n"
            "[skip](https://skip-fallback.test)\n")
    (let ((overlays
           (gptel-ui-links-render
            (point-min) (point-max)
            (lambda (begin _end)
              ;; Reject the enclosing Markdown candidate, but allow its URL.
              (save-excursion
                (goto-char begin)
                (looking-at-p "\\[skip\\]"))))))
      (should
       (equal (mapcar (lambda (ov) (overlay-get ov 'gptel-ui-link)) overlays)
              '("https://markdown-fallback.test"
                "https://org-fallback.test" "https://skip-fallback.test")))
      (should-not (cl-some (lambda (ov) (overlay-get ov 'display)) overlays)))))

(ert-deftest gptel-ui-links-overlap-work-scales-subquadratically ()
  ;; Count operations rather than elapsed time.  The old overlap scan reads
  ;; every earlier overlay's endpoint and quadruples its work when doubled.
  ;; Fixed grammar passes and indexed overlap checks grow at most N log N.
  (cl-labels
      ((measure (count)
         (with-temp-buffer
           (dotimes (index count)
             (insert (format "[[https://org.test/%d]] [md](https://md.test/%d)\n"
                             index index)))
           (let ((endpoint (symbol-function 'overlay-end))
                 (query (symbol-function 'gptel-ui-links--overlap-p))
                 (element (symbol-function 'aref)) (reads 0))
             (cl-letf (((symbol-function 'overlay-end)
                        (lambda (overlay)
                          (cl-incf reads)
                          (funcall endpoint overlay)))
                       ((symbol-function 'gptel-ui-links--overlap-p)
                        (lambda (begin end ranges)
                          ;; Count queries even when byte compilation has
                          ;; inlined the primitive vector reads below.
                          (cl-incf reads)
                          (funcall query begin end ranges)))
                       ((symbol-function 'aref)
                        (lambda (array index)
                          ;; Strings are parsed separately; vector reads
                          ;; measure the indexed interval queries.
                          (when (vectorp array) (cl-incf reads))
                          (funcall element array index))))
               (should (= (* 2 count)
                          (length (gptel-ui-links-render (point-min) (point-max))))))
             reads))))
    (let ((small (measure 64)) (large (measure 128)))
      (should (> small 0))
      (should (< large (* 3 small)))
      (should (< large (* 128 80))))))

(provide 'gptel-ui-links-test)
;;; gptel-ui-links-test.el ends here
