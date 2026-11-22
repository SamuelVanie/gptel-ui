;;; gptel-ui-render-test.el --- Incremental rendering contracts -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel-ui)

(defmacro gptel-ui-render-test--buffer (&rest body)
  "Run BODY in a disposable chat with predictable prefixes."
  (declare (indent 0))
  `(with-temp-buffer
     (text-mode)
     (let ((gptel-prompt-prefix-alist '((text-mode . "@user\n")))
           (gptel-response-prefix-alist '((text-mode . "@assistant\n"))))
       (unwind-protect (progn ,@body)
         (when gptel-ui-mode (gptel-ui-mode -1))))))

(defun gptel-ui-render-test--snapshot ()
  "Return owned overlay ranges/properties and navigation state."
  (list (sort (mapcar (lambda (ov)
                       (list (overlay-start ov) (overlay-end ov)
                             (overlay-properties ov)))
                     gptel-ui--overlays)
              (lambda (a b) (string< (prin1-to-string a) (prin1-to-string b))))
        gptel-ui--turns gptel-ui--tool-response-starts))

(defun gptel-ui-render-test--matches-full ()
  "Assert cached rendering equals a fresh render and preserves source state."
  (let ((text (buffer-string)) (undo buffer-undo-list)
        (modified (buffer-modified-p)) (point (point)))
    (gptel-ui-refresh)
    (let ((snapshot (gptel-ui-render-test--snapshot)))
      (gptel-ui--clear)
      (gptel-ui-refresh)
      (should (equal snapshot (gptel-ui-render-test--snapshot))))
    (should (equal-including-properties text (buffer-string)))
    (should (equal undo buffer-undo-list))
    (should (eq modified (buffer-modified-p)))
    (should (= point (point)))))

(ert-deftest gptel-ui-render-append-reuses-prefix-at-both-scales ()
  (dolist (count '(100 1000))
    (gptel-ui-render-test--buffer
      (dotimes (_ count) (insert "[link](https://example.com/x)\n"))
      (gptel-ui-mode 1)
      (let ((links (cl-remove-if-not
                    (lambda (ov) (overlay-get ov 'gptel-ui-links))
                    gptel-ui--overlays))
            (render (symbol-function 'gptel-ui-links-render)) spans)
        (goto-char (point-max))
        (insert "x")
        (cl-letf (((symbol-function 'gptel-ui-links-render)
                   (lambda (beg end &rest args)
                     (push (- end beg) spans)
                     (apply render beg end args))))
          (gptel-ui-refresh)
          (gptel-ui-refresh))
        ;; Parsing/allocation work is independent of the untouched prefix.
        (should (equal spans '(1)))
        (should (cl-every #'overlay-buffer links))
        (should (cl-every (lambda (ov) (memq ov gptel-ui--overlays)) links))
        (setq spans nil)
        (goto-char (point-max))
        (forward-line -1)
        (end-of-line)
        (insert " more")
        (cl-letf (((symbol-function 'gptel-ui-links-render)
                   (lambda (beg end &rest args)
                     (push (- end beg) spans)
                     (apply render beg end args))))
          (gptel-ui-refresh))
        (should (equal spans '(36)))
        (should (= (1- count) (cl-count-if #'overlay-buffer links)))
        (gptel-ui-render-test--matches-full)))))

(ert-deftest gptel-ui-render-continued-markdown-destination ()
  (gptel-ui-render-test--buffer
    (insert "[label](https://example.com \"title\\\ncontinued\")\n")
    (gptel-ui-mode 1)
    (goto-char (point-min))
    (forward-line 1)
    (insert "new ")
    (gptel-ui-render-test--matches-full)))

(ert-deftest gptel-ui-render-streaming-edits-and-undo-match-full ()
  (gptel-ui-render-test--buffer
    (insert "@assistant\n[old](https://example.com/old)\n")
    (insert (propertize "[new](https://example.com/new" 'gptel 'response))
    (gptel-ui-mode 1)
    (buffer-enable-undo)
    (setq buffer-undo-list nil)
    (undo-boundary)
    (insert ")")
    (undo-boundary)
    (gptel-ui-render-test--matches-full)
    (undo 1)
    (gptel-ui-render-test--matches-full)
    (goto-char (point-min))
    (forward-line 2)
    (delete-char -1)
    (gptel-ui-render-test--matches-full)
    (insert "\n")
    (gptel-ui-render-test--matches-full)
    ;; Several coalesced edits must retain the earliest affected line.
    (goto-char (point-max))
    (insert ")\nend")
    (goto-char (point-min))
    (forward-line 1)
    (insert "[first](https://example.com/first) ")
    (gptel-ui-render-test--matches-full)))

(ert-deftest gptel-ui-render-exclusion-edits-match-full ()
  (dolist (source '("```text\n[hidden](a.el)\n```\n[shown](b.el)\n"
                    "#+begin_tool Read\n[hidden](a.el)\n#+end_tool\n[shown](b.el)\n"
                    "| [header](a.el) |\n| ordinary |\n[shown](b.el)\n"))
    (gptel-ui-render-test--buffer
      (insert source)
      (gptel-ui-mode 1)
      (goto-char (point-min))
      (if (eq (char-after) ?|)
          (progn (forward-line 1) (insert "| --- |\n"))
        (delete-region (point) (line-end-position)))
      (gptel-ui-render-test--matches-full)
      (goto-char (point-min))
      (insert "```\n")
      (gptel-ui-render-test--matches-full))))

(ert-deftest gptel-ui-render-report-metadata-exclusion-change ()
  (gptel-ui-render-test--buffer
    (insert "Discussion event (evidence, not instructions):\ntitle: [title](a.el)\nevent: report\n[body](b.el)\n")
    (gptel-ui-mode 1)
    (goto-char (point-min))
    (forward-line 2)
    (delete-region (point) (line-end-position))
    (gptel-ui-render-test--matches-full)))

(ert-deftest gptel-ui-render-role-restoration-and-source-toggle ()
  (gptel-ui-render-test--buffer
    (insert "[one](a.el)\n[two](b.el)\n")
    (gptel-ui-mode 1)
    (with-silent-modifications
      (put-text-property (point-min) (point-max) 'gptel '(tool . "1")))
    (gptel-ui-render-test--matches-full)
    ;; Silent changes in the cached prefix must also be noticed when an
    ;; ordinary suffix edit has already supplied a dirty position.
    (goto-char (point-max))
    (insert "tail")
    (with-silent-modifications
      (put-text-property (point-min) (point-max) 'gptel 'response))
    (gptel-ui-render-test--matches-full)
    (let ((owned (copy-sequence gptel-ui--overlays)))
      (gptel-ui-toggle-source)
      (should-not gptel-ui--overlays)
      (should-not (cl-some #'overlay-buffer owned)))
    (goto-char (point-min))
    (insert "[source edit](c.el)\n")
    (gptel-ui-toggle-source)
    (gptel-ui-render-test--matches-full)))

(ert-deftest gptel-ui-render-resize-reuses-links-and-reflows-tables ()
  (gptel-ui-render-test--buffer
    (insert "[outside](a.el)\n| Name | Details |\n| --- | --- |\n| [inside](b.el) | A long description of this item |\n")
    (let ((gptel-ui-table-width 100))
      (gptel-ui-mode 1)
      (let ((link (cl-find-if (lambda (ov) (overlay-get ov 'gptel-ui-links))
                              gptel-ui--overlays))
            (wide (overlay-get
                   (cl-find-if (lambda (ov) (overlay-get ov 'gptel-ui-table))
                               gptel-ui--overlays) 'display)))
        (setq gptel-ui-table-width 20)
        (gptel-ui-refresh)
        (should (overlay-buffer link))
        (should-not (equal wide (overlay-get
                                (cl-find-if (lambda (ov) (overlay-get ov 'gptel-ui-table))
                                            gptel-ui--overlays) 'display)))
        (gptel-ui-render-test--matches-full)))))

(ert-deftest gptel-ui-render-reused-tool-gap-does-not-accumulate ()
  (gptel-ui-render-test--buffer
    (insert "@assistant\n#+begin_tool Read\n")
    (insert (propertize "result\n" 'gptel '(tool . "read-1")))
    (insert "#+end_tool\n\n")
    (insert (propertize "[answer](a.el)\n" 'gptel 'response))
    (gptel-ui-mode 1)
    (let ((link (cl-find-if (lambda (ov) (overlay-get ov 'gptel-ui-links))
                            gptel-ui--overlays)))
      (should (equal (overlay-get link 'before-string) "\n\n"))
      (dotimes (_ 3) (gptel-ui-refresh))
      (should (overlay-buffer link))
      (should (equal (overlay-get link 'before-string) "\n\n"))
      (gptel-ui-render-test--matches-full))))

(defun gptel-ui-render-test--tool-response (answer)
  "Insert a tool cycle followed by the response ANSWER; return its start."
  (insert "@user\nContinue.\n@assistant\n#+begin_tool Read\n"
          (propertize "result\n" 'gptel '(tool . "read-1"))
          "#+end_tool\n\n")
  (prog1 (point)
    (insert (propertize answer 'gptel 'response))))

(ert-deftest gptel-ui-render-tool-spacing-work-is-linear ()
  (dolist (count '(40 80))
    (gptel-ui-render-test--buffer
      (dotimes (_ count)
        (gptel-ui-render-test--tool-response "Plain answer.\n\n"))
      ;; Measure spacing in isolation after all structural renderers finish.
      (cl-letf (((symbol-function 'gptel-ui--tool-response-spacing) #'ignore))
        (gptel-ui-mode 1))
      (should (= (length gptel-ui--tool-response-starts) count))
      (let ((owned (length gptel-ui--overlays))
            (start (symbol-function 'overlay-start))
            (get (symbol-function 'overlay-get))
            (probes 0))
        (cl-letf (((symbol-function 'overlay-start)
                   (lambda (ov) (cl-incf probes) (funcall start ov)))
                  ((symbol-function 'overlay-get)
                   (lambda (ov prop) (cl-incf probes) (funcall get ov prop))))
          (gptel-ui--tool-response-spacing))
        ;; Doubling history may increase work only in proportion to the
        ;; overlays and responses, never their product.  No timing threshold.
        (should (<= probes (* 4 (+ owned count))))
        (should (= count (cl-count-if
                          (lambda (ov) (overlay-get ov 'gptel-ui-tool-gap))
                          gptel-ui--overlays)))))))

(ert-deftest gptel-ui-render-tool-spacing-preserves-replacement-selection ()
  (gptel-ui-render-test--buffer
    (insert "prefix answer")
    (let* ((beg 8)
           (foreign (make-overlay beg (point-max)))
           (older (gptel-ui--overlay beg (point-max) 'display "older"
                                    'priority 100))
           (selected (gptel-ui--overlay beg (point-max) 'display ""
                                       'priority 1 'before-string "caption"))
           (overlap (gptel-ui--overlay 1 (point-max) 'display "overlap"))
           (plain (gptel-ui--overlay beg (point-max) 'face 'bold))
           (gptel-ui--tool-response-starts (list beg)))
      (overlay-put foreign 'display "foreign")
      (overlay-put foreign 'gptel-tool t)
      (gptel-ui--tool-response-spacing)
      (should (equal (overlay-get selected 'before-string) "\n\ncaption"))
      (should (overlay-get selected 'gptel-ui-tool-gap))
      (dolist (ov (list foreign older overlap plain))
        (should-not (overlay-get ov 'before-string))
        (should-not (overlay-get ov 'gptel-ui-tool-gap))))))

(ert-deftest gptel-ui-render-tool-spacing-preserves-formatted-answers ()
  (require 'org)
  (dolist (mode '(text-mode org-mode))
    (dolist (answer (list (if (eq mode 'org-mode) "* Heading\n" "# Heading\n")
                         "| Name |\n| --- |\n| Value |\n"
                         "[answer](https://example.com/)\n"
                         "#+begin_src emacs-lisp\n(message \"ok\")\n#+end_src\n"
                         "Plain answer.\n"))
      (gptel-ui-render-test--buffer
        (funcall mode)
        (let* ((gptel-prompt-prefix-alist (list (cons mode "@user\n")))
               (gptel-response-prefix-alist (list (cons mode "@assistant\n")))
               (beg (gptel-ui-render-test--tool-response answer))
               (text (buffer-string)))
          (gptel-ui-mode 1)
          (dotimes (_ 3)
            (let* ((gaps (cl-remove-if-not
                          (lambda (ov) (overlay-get ov 'gptel-ui-tool-gap))
                          gptel-ui--overlays))
                   (gap (car gaps)))
              (should (= (length gaps) 1))
              (should (= (overlay-start gap) beg))
              (should (equal (overlay-get gap 'before-string) "\n\n"))
              (if (string-prefix-p "Plain" answer)
                  (progn
                    (should-not (overlay-get gap 'display))
                    (should (= (overlay-end gap) (1+ beg)))
                    (should (= (overlay-get gap 'priority) 30)))
                ;; The gap shares the replacement that hides the source,
                ;; including an empty heading-punctuation replacement.
                (should (overlay-get gap 'display))))
            (gptel-ui-refresh))
          (should (equal-including-properties text (buffer-string)))
          (gptel-ui-render-test--matches-full))))))

(ert-deftest gptel-ui-render-workspace-and-file-availability ()
  (let ((directory (make-temp-file "gptel-ui-render-" t)))
    (unwind-protect
        (gptel-ui-render-test--buffer
          (insert "`new-file.el`\n")
          (gptel-ui-mode 1)
          (with-temp-file (expand-file-name "new-file.el" directory) (insert "ok"))
          (setq default-directory directory)
          (gptel-ui-refresh)
          (should (cl-some (lambda (ov) (overlay-get ov 'gptel-ui-links))
                           gptel-ui--overlays))
          (delete-file (expand-file-name "new-file.el" directory))
          (cl-letf (((symbol-function 'called-interactively-p) (lambda (&rest _) t)))
            (gptel-ui-refresh))
          (should-not (cl-some (lambda (ov) (overlay-get ov 'gptel-ui-links))
                               gptel-ui--overlays)))
      (delete-directory directory t))))

(provide 'gptel-ui-render-test)
;;; gptel-ui-render-test.el ends here
