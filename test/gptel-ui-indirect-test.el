;;; gptel-ui-indirect-test.el --- Shared-text view refresh -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel-ui)

(defmacro gptel-ui-indirect-test--views (&rest body)
  "Run BODY with enabled BASE, FIRST, SECOND and an independent COPY."
  (declare (indent 0))
  `(let ((base (generate-new-buffer " *ui-shared-base*"))
         (gptel-ui-refresh-delay 3600)
         first second copy)
     (unwind-protect
         (with-current-buffer base
           (text-mode)
           (insert (propertize
                    "[prefix](https://example.com/prefix)\n\n| Result |\n|--------|\n| done   |\n"
                    'gptel 'response))
           (gptel-ui-mode 1)
           (setq first (clone-indirect-buffer " *ui-shared-first*" nil)
                 second (clone-indirect-buffer " *ui-shared-second*" nil)
                 copy (clone-buffer " *ui-independent-copy*" nil))
           ;; Startup timers must not hide missing change notifications.
           (dolist (view (list base first second copy))
             (with-current-buffer view (gptel-ui-refresh)))
           ,@body)
       (dolist (view (list copy second first base))
         (when (buffer-live-p view) (kill-buffer view))))))

(defun gptel-ui-indirect-test--flush (view)
  "Run VIEW's scheduled callback and check that rendering preserves text."
  (with-current-buffer view
    (let ((timer gptel-ui--timer)
          (text (save-restriction (widen) (buffer-string)))
          (undo buffer-undo-list)
          (modified (buffer-modified-p))
          (tick (buffer-modified-tick))
          (position (point))
          (bounds (cons (point-min) (point-max))))
      (should (timerp timer))
      (apply (timer--function timer) (timer--args timer))
      (should-not gptel-ui--timer)
      (should-not (memq timer timer-list))
      (should (equal-including-properties
               text (save-restriction (widen) (buffer-string))))
      (should (equal undo buffer-undo-list))
      (should (eq modified (buffer-modified-p)))
      (should (= tick (buffer-modified-tick)))
      (should (= position (point)))
      (should (equal bounds (cons (point-min) (point-max)))))))

(defun gptel-ui-indirect-test--targets ()
  "Return the current view's sorted ordinary link targets."
  (sort (cl-loop for ov in gptel-ui--overlays
                 when (overlay-get ov 'gptel-ui-links)
                 collect (overlay-get ov 'gptel-ui-link))
        #'string<))

(ert-deftest gptel-ui-indirect-stream-refreshes-tables-and-links ()
  (dolist (origin '(base first second))
    (gptel-ui-indirect-test--views
      (let* ((views (list base first second))
             (editor (pcase origin ('base base) ('first first) (_ second)))
             (prefixes (mapcar
                        (lambda (view)
                          (with-current-buffer view
                            (cl-find-if (lambda (ov) (overlay-get ov 'gptel-ui-links))
                                        gptel-ui--overlays)))
                        views))
             (answer (point-max)))
        (buffer-enable-undo)
        (setq buffer-undo-list nil)
        (with-current-buffer editor
          (goto-char (point-max))
          (insert (propertize "\nFINAL ANSWER [details](https://example.com/final).\n"
                              'gptel 'response)))
        (let ((timers (mapcar (lambda (view) (buffer-local-value 'gptel-ui--timer view))
                              views)))
          (should (cl-every #'timerp timers))
          (dotimes (_ 3)
            (with-current-buffer editor
              (goto-char (point-max))
              (insert "more ")))
          (should (equal timers (mapcar
                                (lambda (view) (buffer-local-value 'gptel-ui--timer view))
                                views))))
        (should-not (buffer-local-value 'gptel-ui--timer copy))
        (cl-mapc
         (lambda (view prefix)
           (gptel-ui-indirect-test--flush view)
           (with-current-buffer view
             (should-not (get-char-property (1+ answer) 'display))
             (should (equal (gptel-ui-indirect-test--targets)
                            '("https://example.com/final" "https://example.com/prefix")))
             ;; Shared edits retain incremental link parsing in each view.
             (should (memq prefix gptel-ui--overlays))
             (should (overlay-buffer prefix))))
         views prefixes)
        (with-current-buffer copy
          (should-not (string-match-p "FINAL ANSWER" (buffer-string))))))))

(ert-deftest gptel-ui-indirect-edits-through-disabled-views ()
  (dolist (origin '(base first))
    (gptel-ui-indirect-test--views
      (let ((editor (if (eq origin 'base) base first)))
        (with-current-buffer editor
          (gptel-ui-mode -1)
          (goto-char (point-max))
          (insert "\n[disabled edit](https://example.com/disabled)\n")
          (should-not gptel-ui--timer))
        (dolist (view (delq editor (list base first second)))
          (gptel-ui-indirect-test--flush view)
          (with-current-buffer view
            (should (member "https://example.com/disabled"
                            (gptel-ui-indirect-test--targets)))))
        (should-not (buffer-local-value 'gptel-ui--timer copy))))))

(ert-deftest gptel-ui-indirect-narrowing-and-coalesced-earlier-edits ()
  (gptel-ui-indirect-test--views
    (with-current-buffer second
      (goto-char (point-max))
      (forward-line -1)
      (narrow-to-region (point) (point-max)))
    (goto-char (point-max))
    (insert "\n[tail](https://example.com/tail)\n")
    (goto-char (point-min))
    (delete-region (point) (line-end-position))
    (insert "[new prefix](https://example.com/new)")
    (dolist (view (list base first second))
      (gptel-ui-indirect-test--flush view)
      (with-current-buffer view
        (should (equal (gptel-ui-indirect-test--targets)
                       '("https://example.com/new" "https://example.com/tail")))))))

(ert-deftest gptel-ui-indirect-source-view-stays-undecorated ()
  (gptel-ui-indirect-test--views
    (with-current-buffer first (gptel-ui-toggle-source))
    (with-current-buffer first
      (goto-char (point-max))
      (insert "\n[source edit](https://example.com/source)\n"))
    (dolist (view (list base first second))
      (gptel-ui-indirect-test--flush view))
    (with-current-buffer first
      (should gptel-ui--source)
      (should-not gptel-ui--overlays)
      (gptel-ui-toggle-source)
      (should (member "https://example.com/source" (gptel-ui-indirect-test--targets))))
    (with-current-buffer second
      (should (member "https://example.com/source" (gptel-ui-indirect-test--targets))))))

(ert-deftest gptel-ui-indirect-teardown-keeps-surviving-views-current ()
  (gptel-ui-indirect-test--views
    (goto-char (point-max))
    (insert "\n[tail](https://example.com/tail)\n")
    (let ((timer (buffer-local-value 'gptel-ui--timer first)))
      (should (timerp timer))
      (kill-buffer first)
      (should-not (memq timer timer-list)))
    (with-current-buffer base (fundamental-mode))
    (should-not gptel-ui--timer)
    (goto-char (point-max))
    (insert "[later](https://example.com/later)\n")
    (gptel-ui-indirect-test--flush second)
    (with-current-buffer second
      (should (member "https://example.com/later" (gptel-ui-indirect-test--targets))))
    (should-not (buffer-local-value 'gptel-ui--timer copy))))

(ert-deftest gptel-ui-indirect-cloned-overlays-are-owned ()
  (with-temp-buffer
    (insert "[link](https://example.com)\n\n| Header |\n| --- |\n| cell |\n")
    (insert "\n``` tool (Read \"README\")\n")
    (insert (propertize "(:name \"Read\" :args (\"README\"))\n\nbody\n"
                        'gptel '(tool . "1")))
    (insert "```\n")
    (let ((native (make-overlay (point-min) (point-min)))
          (fold (make-overlay (- (point-max) 9) (- (point-max) 4)))
          (map (make-sparse-keymap))
          (gptel-ui-refresh-delay 3600)
          clone)
      (define-key map (kbd "C-c C-c") #'ignore)
      (overlay-put native 'gptel-tool '(pending))
      (overlay-put native 'keymap map)
      (overlay-put fold 'invisible 'gptel)
      (unwind-protect
          (progn
            (gptel-ui-mode 1)
            (should (cl-some (lambda (ov) (overlay-get ov 'gptel-ui-tool))
                             gptel-ui--overlays))
            (let ((originals (copy-sequence gptel-ui--overlays)))
              (setq clone (clone-indirect-buffer " *ui-overlay-clone*" nil))
              (should (cl-every #'overlay-buffer originals))
              (with-current-buffer clone
                (let ((overlays (append (car (overlay-lists)) (cdr (overlay-lists)))))
                  (should (cl-some (lambda (ov) (overlay-get ov 'gptel-tool)) overlays))
                  (dolist (ov overlays)
                    (when (or (overlay-get ov 'gptel-ui)
                              (overlay-get ov 'gptel-ui-links)
                              (overlay-get ov 'gptel-ui-tables)
                              (overlay-get ov 'gptel-ui-tool))
                      (should (memq ov gptel-ui--overlays)))))
                (gptel-ui-toggle-source)
                (should-not gptel-ui--overlays)
                (should-not (get-char-property (point-min) 'display))
                (gptel-ui-toggle-source)
                (gptel-ui-mode -1)
                (let ((overlays (append (car (overlay-lists)) (cdr (overlay-lists)))))
                  (should-not (cl-some
                               (lambda (ov) (or (overlay-get ov 'gptel-ui)
                                                (overlay-get ov 'gptel-ui-links)
                                                (overlay-get ov 'gptel-ui-tables)
                                                (overlay-get ov 'gptel-ui-tool)))
                               overlays))
                  (should (cl-some (lambda (ov)
                                     (and (overlay-get ov 'gptel-tool)
                                          (eq (overlay-get ov 'keymap) map)))
                                   overlays))
                  (should (cl-some (lambda (ov) (eq (overlay-get ov 'invisible) 'gptel))
                                   overlays))))
              (should (cl-every #'overlay-buffer originals)))
            (should (overlay-buffer native))
            (should (eq (overlay-get native 'keymap) map))
            (should (eq (overlay-get fold 'invisible) 'gptel)))
        (when (buffer-live-p clone) (kill-buffer clone))
        (gptel-ui-mode -1)))))

(ert-deftest gptel-ui-indirect-dispatch-scales-with-related-views ()
  (gptel-ui-indirect-test--views
    (let ((schedule (symbol-function 'gptel-ui--schedule)) calls)
      ;; Edits must not search all buffers or eagerly run any renderer.
      (cl-letf (((symbol-function 'buffer-list)
                 (lambda (&rest _) (ert-fail "Edit scanned every buffer")))
                ((symbol-function 'gptel-ui-refresh)
                 (lambda (&rest _) (ert-fail "Edit rendered synchronously")))
                ((symbol-function 'gptel-ui--schedule)
                 (lambda (&rest args)
                   (push (current-buffer) calls)
                   (apply schedule args))))
        (with-temp-buffer (insert "Unrelated text"))
        (should-not calls)
        (goto-char (point-max))
        (insert "\nanswer"))
      (should (= (length calls) 3))
      (should (cl-every (lambda (view) (memq view calls)) (list base first second))))))

(ert-deftest gptel-ui-indirect-base-teardown-releases-watcher ()
  (let ((groups (hash-table-count gptel-ui--text-views))
        (watching (memq #'gptel-ui--other-text-changed (default-value 'after-change-functions))))
    (gptel-ui-indirect-test--views
      (goto-char (point-max))
      (insert "\nanswer")
      (let ((timers (mapcar (lambda (view) (buffer-local-value 'gptel-ui--timer view))
                            (list base first second))))
        (kill-buffer base)
        (should-not (buffer-live-p first))
        (should-not (buffer-live-p second))
        (should-not (gethash base gptel-ui--text-views))
        (should-not (cl-some (lambda (timer) (memq timer timer-list)) timers))))
    (should (= groups (hash-table-count gptel-ui--text-views)))
    (should (eq (not watching)
                (not (memq #'gptel-ui--other-text-changed
                           (default-value 'after-change-functions)))))))

(ert-deftest gptel-ui-indirect-local-hooks-without-defaults ()
  (with-temp-buffer
    (setq-local after-change-functions '(ignore))
    (let ((gptel-ui-refresh-delay 3600) clone)
      (unwind-protect
          (progn
            (gptel-ui-mode 1)
            (setq clone (clone-indirect-buffer " *ui-local-hooks*" nil))
            (gptel-ui-refresh)
            (with-current-buffer clone (gptel-ui-refresh))
            (insert "[link](https://example.com/new)")
            (dolist (view (list (current-buffer) clone))
              (gptel-ui-indirect-test--flush view)
              (with-current-buffer view
                (should (equal (gptel-ui-indirect-test--targets)
                               '("https://example.com/new"))))))
        (when (buffer-live-p clone) (kill-buffer clone))
        (gptel-ui-mode -1))
      (should (equal after-change-functions '(ignore))))))

(provide 'gptel-ui-indirect-test)
;;; gptel-ui-indirect-test.el ends here
