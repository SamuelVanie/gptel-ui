;;; gptel-ui-compat-test.el --- Chat view migration contracts -*- lexical-binding: t; -*-

;;; Commentary:

;; Exercise existing configuration and commands against the relocated package.
;; Fresh Emacs processes verify initialization without loading runner libraries.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gptel-ui)

(defconst gptel-ui-compat-test--directory
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Directory containing the standalone UI package.")

(defun gptel-ui-compat-test--fresh-emacs (&rest forms)
  "Evaluate FORMS in a fresh Emacs with only this package on its load path."
  (with-temp-buffer
    (let ((status
           (process-file
            (expand-file-name invocation-name invocation-directory)
            nil (current-buffer) nil
            "-Q" "--batch" "-L" gptel-ui-compat-test--directory
            "--eval"
            (prin1-to-string
             `(progn
                (setq load-prefer-newer t)
                (require 'ert)
                ,@forms)))))
      (ert-info ((buffer-string))
        (should (equal status 0))))))

(ert-deftest gptel-ui-compat-preserves-settings-before-first-load ()
  (gptel-ui-compat-test--fresh-emacs
   '(setq gptel-runner-chat-width 73
          gptel-runner-chat-refresh-delay 0.42
          gptel-runner-chat-assistant-name "Guide"
          gptel-runner-chat-table-width 43
          gptel-runner-chat-mode-map (make-sparse-keymap)
          gptel-runner-chat-mode-hook '(ignore))
   '(define-key gptel-runner-chat-mode-map (kbd "C-c z") #'ignore)
   '(require 'gptel-ui)
   '(should (= gptel-ui-width 73))
   '(should (= gptel-ui-refresh-delay 0.42))
   '(should (equal gptel-ui-assistant-name "Guide"))
   '(should (= gptel-ui-table-width 43))
   '(should (eq (lookup-key gptel-ui-mode-map (kbd "C-c z")) #'ignore))
   '(should (equal gptel-ui-mode-hook '(ignore))))
  (gptel-ui-compat-test--fresh-emacs
   '(custom-set-variables '(gptel-runner-chat-width 81)
                          '(gptel-runner-chat-table-width 47))
   '(require 'gptel-runner-chat)
   '(should (= gptel-ui-width 81))
   '(should (= gptel-ui-table-width 47))))

(ert-deftest gptel-ui-compat-preserves-saved-faces-before-first-load ()
  (gptel-ui-compat-test--fresh-emacs
   '(custom-set-faces
     '(gptel-runner-chat-code ((t (:background "#123456"))))
     '(gptel-runner-chat-table-header ((t (:weight light)))))
   '(require 'gptel-ui)
   '(should (equal (face-attribute 'gptel-ui-code :background nil t)
                   "#123456"))
   '(should (eq (face-attribute 'gptel-ui-table-header :weight nil t) 'light))
   '(should (equal (face-attribute 'gptel-runner-chat-code :background nil t)
                   "#123456"))))

(ert-deftest gptel-ui-compat-loaders-work-without-runner ()
  (dolist (entry '((gptel-runner-chat . gptel-ui)
                   (gptel-runner-chat-links . gptel-ui-links)
                   (gptel-runner-chat-tables . gptel-ui-tables)
                   (gptel-runner-chat-tools . gptel-ui-tools)))
    (gptel-ui-compat-test--fresh-emacs
     `(require ',(car entry))
     `(should (featurep ',(cdr entry)))
     '(should-not (featurep 'gptel-runner))
     '(should-not (featurep 'gptel-runner-core))
     '(should-not (featurep 'gptel-runner-gptel)))))

(ert-deftest gptel-ui-compat-old-and-new-commands-share-one-mode ()
  (require 'gptel-runner-chat)
  (with-temp-buffer
    (text-mode)
    (insert "@user\nInspect this.\n\n@assistant\n")
    (insert (propertize "# Result\n\nThe source stays intact.\n" 'gptel 'response))
    (buffer-enable-undo)
    (set-buffer-modified-p nil)
    (let ((text (buffer-string))
          (undo buffer-undo-list)
          (hook-calls 0)
          (gptel-prompt-prefix-alist '((text-mode . "@user\n")))
          (gptel-response-prefix-alist '((text-mode . "@assistant\n"))))
      (setq-local gptel-runner-chat-mode-hook
                  (list (lambda () (cl-incf hook-calls))))
      (unwind-protect
          (progn
            (gptel-runner-chat-mode 1)
            (should gptel-ui-mode)
            (should gptel-runner-chat-mode)
            (should (= hook-calls 1))
            (should gptel-ui--overlays)
            (should (eq gptel-runner-chat-mode-map gptel-ui-mode-map))
            (should (eq gptel-runner-chat-links-map gptel-ui-links-map))
            (should (eq gptel-runner-chat-tools-map gptel-ui-tools-map))
            (gptel-runner-chat-toggle-source)
            (should-not gptel-ui--overlays)
            (gptel-ui-toggle-source)
            (should gptel-ui--overlays)
            (gptel-ui-mode -1)
            (should-not gptel-runner-chat-mode)
            (should (= hook-calls 2))
            (should-not gptel-ui--overlays)
            (should-not gptel-ui--timer)
            (should (equal-including-properties text (buffer-string)))
            (should (equal undo buffer-undo-list))
            (should-not (buffer-modified-p)))
        (when gptel-ui-mode (gptel-ui-mode -1))))))

(ert-deftest gptel-ui-compat-old-options-retain-buffer-local-behavior ()
  (let ((width gptel-ui-width))
    (with-temp-buffer
      (setq-local gptel-runner-chat-width 61)
      (should (= gptel-ui-width 61))
      (should (local-variable-p 'gptel-ui-width))
      (setq-local gptel-ui-width 67)
      (should (= gptel-runner-chat-width 67)))
    (should (= gptel-ui-width width))))

(ert-deftest gptel-ui-compat-old-faces-customize-current-rendering ()
  (let ((weight (face-attribute 'gptel-ui-heading :weight)))
    (unwind-protect
        (progn
          (set-face-attribute 'gptel-runner-chat-heading nil :weight 'light)
          (should (eq (face-attribute 'gptel-ui-heading :weight) 'light)))
      (set-face-attribute 'gptel-ui-heading nil :weight weight))))

(provide 'gptel-ui-compat-test)
;;; gptel-ui-compat-test.el ends here
