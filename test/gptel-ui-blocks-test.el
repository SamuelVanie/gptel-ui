;;; gptel-ui-blocks-test.el --- Tool and code block boundaries -*- lexical-binding: t; -*-

;;; Commentary:
;; Check native tool boundaries without requiring gptel or a model request.

;;; Code:

(require 'ert)
(require 'gptel-ui)

(defmacro gptel-ui-blocks-test--buffer (&rest body)
  "Run BODY in a disposable chat with no speaker prefixes."
  (declare (indent 0))
  `(with-temp-buffer
     (text-mode)
     (let ((gptel-prompt-prefix-alist nil)
           (gptel-response-prefix-alist nil))
       (unwind-protect (progn ,@body)
         (when gptel-ui-mode (gptel-ui-mode -1))))))

(defun gptel-ui-blocks-test--code-ranges ()
  "Return the ranges styled as code panels."
  (cl-loop for ov in gptel-ui--overlays
           when (eq (overlay-get ov 'face) 'gptel-ui-code)
           collect (cons (overlay-start ov) (overlay-end ov))))

(defun gptel-ui-blocks-test--answer-visible (beg)
  "Assert the heading and link in the answer at BEG are rendered."
  (should-not (gptel-ui--excluded-p beg (point-max)))
  (should (= 1 (cl-count-if
                (lambda (ov) (eq (overlay-get ov 'face) 'gptel-ui-heading))
                gptel-ui--overlays)))
  (should (= 1 (cl-count-if (lambda (ov) (overlay-get ov 'gptel-ui-link))
                          gptel-ui--overlays))))

(ert-deftest gptel-ui-blocks-unfinished-native-tool-keeps-nested-fences ()
  (gptel-ui-blocks-test--buffer
    (insert (propertize "``` tool (Read)" 'gptel 'ignore)
            (propertize "\n(:name \"Read\" :args nil)\n\n```elisp\n(+ 1 2)\n```\n"
                        'gptel (cons 'tool "read-1")))
    (gptel-ui-mode 1)
    (dotimes (chunk 2)
      (when (= chunk 1)
        (goto-char (point-max))
        ;; Equal roles can be distinct property objects across streamed chunks.
        (insert (propertize "More output\n```\n" 'gptel (cons 'tool "read-1")))
        (gptel-ui-refresh))
      (should (equal gptel-ui--excluded (list (cons (point-min) (point-max)))))
      (should-not (gptel-ui-blocks-test--code-ranges))
      (let ((header (cl-find-if (lambda (ov) (overlay-get ov 'gptel-ui-tool-header))
                                gptel-ui--overlays)))
        (should header)
        (should-not (overlay-get header 'keymap))))
    (goto-char (point-max))
    (insert (propertize "\n```\n" 'gptel 'ignore))
    (let ((answer (point)))
      (insert (propertize "# Summary\n[Result](https://example.com/result)\n"
                          'gptel 'response))
      (let ((source (buffer-string)))
        (gptel-ui-refresh)
        (gptel-ui-blocks-test--answer-visible answer)
        (should-not (gptel-ui-blocks-test--code-ranges))
        (should (equal-including-properties source (buffer-string)))))))

(ert-deftest gptel-ui-blocks-missing-tool-close-stops-before-assistant-code ()
  (gptel-ui-blocks-test--buffer
    (insert (propertize "``` tool (Read)" 'gptel 'ignore)
            (propertize "\n(:name \"Read\" :args nil)\n\nResult\n"
                        'gptel '(tool . "read-1")))
    (let ((tool-end (point)) (code-beg (point)))
      ;; A bare assistant fence must not become the missing tool close.
      (insert (propertize "```\n# Literal\n[Hidden](https://example.com/hidden)\n```"
                          'gptel 'response))
      (let ((code-end (point)))
        (insert (propertize "\n" 'gptel 'response))
        (let ((answer (point)))
          (insert (propertize "# Summary\n[Result](https://example.com/result)\n"
                              'gptel 'response))
          (gptel-ui-mode 1)
          (should (member (cons (point-min) tool-end) gptel-ui--excluded))
          (should (equal (gptel-ui-blocks-test--code-ranges)
                         (list (cons code-beg code-end))))
          (gptel-ui-blocks-test--answer-visible answer))))))

(ert-deftest gptel-ui-blocks-ordinary-assistant-code-keeps-fence-rules ()
  (dolist (fences '(("```elisp" . "```")
                    ("~~~~text" . "~~~~~")
                    ("````text" . "````")
                    ("#+begin_src elisp" . "#+end_src")
                    ("#+begin_example" . "#+end_example")))
    (ert-info ((format "Fences %S" fences))
      (gptel-ui-blocks-test--buffer
        (insert (propertize
                 (concat (car fences) "\n# Literal\n[Hidden](https://example.com/hidden)\n"
                         (when (equal (car fences) "````text") "```\n")
                         (cdr fences))
                 'gptel 'response))
        (let ((code-end (point)))
          (insert (propertize "\n" 'gptel 'response))
          (let ((answer (point)))
            (insert (propertize "# Summary\n[Result](https://example.com/result)\n"
                                'gptel 'response))
            (gptel-ui-mode 1)
            (should (equal (gptel-ui-blocks-test--code-ranges)
                           (list (cons (point-min) code-end))))
            (gptel-ui-blocks-test--answer-visible answer)))))))

(ert-deftest gptel-ui-blocks-interrupted-code-stays-in-its-message ()
  (dolist (pair '(("```elisp" . "```") ("~~~~txt" . "~~~~")
                  ("```tool" . "```")
                  ("#+begin_src elisp" . "#+end_src")
                  ("#+begin_example" . "#+end_example")))
    (ert-info ((format "Delimiters %S" pair))
      (gptel-ui-blocks-test--buffer
        (let ((gptel-prompt-prefix-alist '((text-mode . "@user\n")))
              first end next answer later close)
          (insert "@user\nFirst question.\n\n")
          (setq first (point))
          ;; A literal prompt inside the unfinished block is still code.
          (insert (propertize (concat (car pair) "\n@user\npartial") 'gptel 'response))
          (setq end (point))
          (insert "\n\n")
          (setq next (point))
          (insert "@user\nNext question.\n\n")
          (setq answer (point))
          (insert (propertize "[Result](https://example.org/result)\n" 'gptel 'response))
          (setq later (point))
          (insert (propertize (concat (car pair) "\nlater\n" (cdr pair)) 'gptel 'response))
          (setq close (point))
          (let ((source (buffer-string)))
            (gptel-ui-mode 1)
            (dotimes (_ 2)
              (should (equal gptel-ui--excluded
                             (list (cons later close) (cons first end))))
              (should (equal gptel-ui--turns (list (point-min) first next answer)))
              (goto-char first)
              (call-interactively (key-binding (kbd "C-c C-n")))
              (should (= (point) next))
              (call-interactively (key-binding (kbd "C-c C-p")))
              (should (= (point) first))
              (should (equal (get-char-property answer 'gptel-ui-link)
                             "https://example.org/result"))
              (gptel-ui-toggle-source)
              (should-not gptel-ui--excluded)
              (should-not (get-char-property answer 'gptel-ui-link))
              (should (equal-including-properties source (buffer-string)))
              (gptel-ui-toggle-source))))))))

(ert-deftest gptel-ui-blocks-interrupted-prompt-and-native-reasoning ()
  (gptel-ui-blocks-test--buffer
    (insert "```elisp\nunfinished\n\n")
    (let ((reasoning (point)))
      ;; gptel marks the reasoning wrapper and its body independently.
      (insert (propertize "``` reasoning\n" 'gptel 'ignore)
              (propertize "[Literal](ignored.md)" 'gptel 'response)
              (propertize "\n```" 'gptel 'ignore))
      (let ((end (point)))
        (insert "\n\n")
        (let ((answer (point)))
          (insert (propertize "# Summary\n[Result](https://example.org/result)\n"
                              'gptel 'response))
          (gptel-ui-mode 1)
          (should (equal gptel-ui--excluded
                         (list (cons reasoning end) (cons (point-min) reasoning))))
          (gptel-ui-blocks-test--answer-visible answer))))))

(provide 'gptel-ui-blocks-test)
;;; gptel-ui-blocks-test.el ends here
