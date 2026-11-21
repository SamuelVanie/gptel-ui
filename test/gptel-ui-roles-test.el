;;; gptel-ui-roles-test.el --- Empty-prefix role boundaries -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-extra)
(require 'gptel-ui)

(defvar gptel-prompt-prefix-alist)
(defvar gptel-response-prefix-alist)

(ert-deftest gptel-ui-roles-empty-prefix-preserves-turns-and-tool-continuations ()
  (dolist (prompt '(nil ""))
    (dolist (response '(nil "" "@assistant\n" "### "))
      (ert-info ((format "Prompt %S, response %S" prompt response))
        (with-temp-buffer
          (text-mode)
          (let ((gptel-prompt-prefix-alist (list (cons 'text-mode prompt)))
                (gptel-response-prefix-alist (list (cons 'text-mode response)))
                turns final continuation)
            (push (point) turns)
            (insert "First user.\n\n")
            (push (point) turns)
            (insert (or response "")
                    (propertize "First answer.\n\n" 'gptel 'response))
            (insert "#+begin_tool Read\n"
                    (propertize "(:name \"Read\" :args nil)\n\nFile contents\n"
                                'gptel '(tool . "read-1"))
                    "#+end_tool\n\n")
            (setq continuation (point))
            (insert (or response ""))
            (setq final (point))
            (insert (propertize "Continued answer.\n\n" 'gptel 'response))
            (push (point) turns)
            (insert "Second user.\n\n")
            (push (point) turns)
            (insert (or response "")
                    (propertize "Second answer.\n" 'gptel 'response))
            (let ((source (buffer-string)))
              (unwind-protect
                  (progn
                    (gptel-ui-mode 1)
                    (gptel-ui-refresh)
                    (should (equal gptel-ui--turns (nreverse turns)))
                    (should (equal gptel-ui--tool-response-starts (list final)))
                    (when (and response (not (string-empty-p response)))
                      (should (cl-some
                               (lambda (ov) (overlay-get ov 'gptel-ui-continuation))
                               (overlays-at continuation))))
                    (should (equal-including-properties source (buffer-string))))
                (when gptel-ui-mode (gptel-ui-mode -1))))))))))

(defun gptel-ui-roles-test--boundary-work (count response)
  "Count boundary lookup and predicate work for COUNT turns with RESPONSE.
Counting visited candidates detects repeated transcript scans without relying
on elapsed time, allocation details, or a particular lookup data structure."
  (with-temp-buffer
    (text-mode)
    (let ((gptel-prompt-prefix-alist '((text-mode . "")))
          (gptel-response-prefix-alist (list (cons 'text-mode response)))
          (some (symbol-function 'cl-some))
          (excluded (symbol-function 'gptel-ui--excluded-p))
          (work 0))
      (dotimes (_ count)
        (insert "User.\n\n" (or response "")
                (propertize "Answer.\n\n" 'gptel 'response)))
      (cl-letf (((symbol-function 'cl-some)
                 (lambda (predicate &rest sequences)
                   (apply some
                          (lambda (&rest values)
                            (cl-incf work)
                            (apply predicate values))
                          sequences)))
                ((symbol-function 'gptel-ui--excluded-p)
                 (lambda (beg end)
                   (cl-incf work)
                   (funcall excluded beg end))))
        (gptel-ui--roles))
      (should (= (length gptel-ui--turns) (* 2 count)))
      work)))

(ert-deftest gptel-ui-roles-empty-prefix-boundary-work-scales-subquadratically ()
  (dolist (response '(nil "" "@assistant\n"))
    (ert-info ((format "Response prefix %S" response))
      (let ((small (gptel-ui-roles-test--boundary-work 64 response))
            (large (gptel-ui-roles-test--boundary-work 256 response)))
        ;; Four times the transcript may cost modestly more than four times
        ;; the work, but must not approach the sixteenfold quadratic growth.
        (should (> small 0))
        (should (< large (* 6 small)))))))

(provide 'gptel-ui-roles-test)
;;; gptel-ui-roles-test.el ends here
