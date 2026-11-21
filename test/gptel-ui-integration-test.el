;;; gptel-ui-integration-test.el --- Native gptel integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Run separately with real gptel and markdown-mode on load-path.  These tests
;; exercise native headers, mode lifecycle, folding and pending tool controls
;; without sending a request or loading the runner.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'markdown-mode)
(require 'gptel)
(require 'gptel-ui)

(defmacro gptel-ui-integration-test--chat (mode &rest body)
  "Run BODY in an isolated native gptel chat using major MODE."
  (declare (indent 1))
  `(let ((gptel-mode-hook nil)
         (gptel-ui-global-mode nil)
         (gptel-use-header-line t))
     (with-temp-buffer
       (funcall ,mode)
       (setq-local header-line-format "Original header")
       (cl-letf (((symbol-function 'gptel-request)
                  (lambda (&rest _) (ert-fail "Unexpected model request"))))
         (unwind-protect
             (progn (gptel-mode 1) ,@body)
           (when gptel-ui-mode (gptel-ui-mode -1))
           (when gptel-mode (gptel-mode -1)))))))

(ert-deftest gptel-ui-integration-standalone-without-runner ()
  (dolist (feature '(gptel-runner gptel-runner-core gptel-runner-flow
                    gptel-runner-jobs gptel-runner-gptel
                    gptel-runner-discussion))
    (should-not (featurep feature)))
  (should (featurep 'gptel-ui))
  (should (featurep 'gptel)))

(ert-deftest gptel-ui-integration-native-header-status-survives ()
  (dolist (mode '(org-mode markdown-mode))
    (gptel-ui-integration-test--chat mode
      (let ((native header-line-format))
        (should (consp native))
        (gptel-ui-mode 1)
        ;; Native status updates mutate the same tail, including its controls.
        (should (eq (cdr native) (cdr header-line-format)))
        (gptel--update-status " Streaming" 'success)
        (should (equal (substring-no-properties (nth 1 native)) " Streaming"))
        (should (equal (nthcdr 2 header-line-format) (nthcdr 2 native)))
        (gptel-ui-mode -1)
        (should (eq header-line-format native))
        (should (equal (substring-no-properties (nth 1 header-line-format))
                       " Streaming"))
        (gptel-mode -1)
        (should (equal header-line-format "Original header"))))))

(ert-deftest gptel-ui-integration-global-existing-future-and-detached-chats ()
  (let ((gptel-mode-hook nil)
        (gptel-ui-global-mode nil)
        (gptel-use-header-line t)
        (existing (generate-new-buffer " *ui-existing*"))
        (future (generate-new-buffer " *ui-future*")))
    (unwind-protect
        (progn
          (with-current-buffer existing
            (org-mode)
            (setq-local header-line-format "Existing original")
            (gptel-mode 1)
            (should-not gptel-ui-mode))
          (gptel-ui-global-mode 1)
          (should (buffer-local-value 'gptel-ui-mode existing))
          (with-current-buffer future
            (markdown-mode)
            (setq-local header-line-format "Future original")
            (gptel-mode 1)
            (should gptel-ui-mode)
            (gptel-mode -1)
            (should-not gptel-ui-mode)
            (should-not gptel-ui--timer)
            (should-not gptel-ui--overlays)
            (should (equal header-line-format "Future original")))
          (gptel-ui-global-mode -1)
          (with-current-buffer existing
            (should gptel-mode)
            (should-not gptel-ui-mode)
            (should-not gptel-ui--timer)
            (should-not (memq #'gptel-ui--schedule after-change-functions))
            (should-not (memq #'gptel-ui--schedule gptel-post-stream-hook))
            (should-not (memq #'gptel-ui--schedule gptel-post-response-functions)))
          (should-not (memq #'gptel-ui--maybe-enable gptel-mode-hook))
          (should-not (memq #'gptel-ui--window-change window-size-change-functions))
          (should-not (memq #'gptel-ui--window-change
                            window-configuration-change-hook)))
      (gptel-ui-global-mode -1)
      (dolist (buffer (list existing future))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when gptel-mode (gptel-mode -1)))
          (kill-buffer buffer))))))

(ert-deftest gptel-ui-integration-disable-restores-native-highlight ()
  (gptel-ui-integration-test--chat #'org-mode
    (insert (propertize "A native response.\n" 'gptel 'response))
    (let ((gptel-highlight-methods '(face)))
      (gptel-highlight-mode 1)
      (should gptel-highlight-mode)
      (gptel-ui-mode 1)
      (should-not gptel-highlight-mode)
      (should gptel-ui--face-cookie)
      (gptel-ui-mode -1)
      (should gptel-highlight-mode)
      (should-not gptel-ui--face-cookie)
      (should-not gptel-ui--saved)
      (should-not gptel-ui--timer)
      (gptel-highlight-mode -1))))

(ert-deftest gptel-ui-integration-native-tool-folds-remain-operable ()
  (dolist (mode '(org-mode markdown-mode))
    (gptel-ui-integration-test--chat mode
      (insert (if (eq mode 'org-mode)
                  "#+begin_tool (Read \"README.md\")\n"
                "``` tool (Read \"README.md\")\n"))
      (let ((body (point)))
        (insert (propertize "(:name \"Read\" :args (\"README.md\"))\n\nContents\n"
                            'gptel '(tool . "read-1")))
        (insert (if (eq mode 'org-mode) "#+end_tool\n" "```\n"))
        (goto-char (point-min))
        (if (eq mode 'org-mode) (org-cycle) (gptel-markdown-cycle-block))
        (should (invisible-p body))
        (let ((source (buffer-string)))
          (gptel-ui-mode 1)
          (should (invisible-p body))
          (should-not (invisible-p (point-min)))
          (gptel-ui-tools-toggle)
          (should-not (invisible-p body))
          (gptel-ui-tools-toggle)
          (should (invisible-p body))
          (gptel-ui-mode -1)
          (should (invisible-p body))
          (should (equal-including-properties source (buffer-string))))))))

(ert-deftest gptel-ui-integration-native-approval-controls-survive ()
  (dolist (mode '(org-mode markdown-mode))
    (gptel-ui-integration-test--chat mode
      (insert (propertize "Inspecting the project.\n\n" 'gptel 'response))
      (let* ((tool (gptel-make-tool
                    :name "UIIntegrationRead" :description "Read a fixture"
                    :function #'ignore :args nil))
             (position (copy-marker (point)))
             (info (list :position position
                         :backend (gptel--make-backend :name "UI integration")))
             (calls (list (list tool nil #'ignore))))
        ;; This is gptel's actual pending-approval overlay, without execution.
        (gptel--display-tool-calls calls info)
        (let* ((native (cl-find-if (lambda (ov) (overlay-get ov 'gptel-tool))
                                   (overlays-in (point-min) (point-max))))
               (source (buffer-string)))
          (should native)
          (goto-char (overlay-start native))
          (gptel-ui-mode 1)
          (gptel-ui-refresh)
          (should (eq (key-binding (kbd "C-c C-c")) #'gptel--accept-tool-calls))
          (should (eq (key-binding (kbd "C-c C-k")) #'gptel--reject-tool-calls))
          (should (eq (overlay-get native 'keymap) gptel-tool-call-actions-map))
          (should (equal-including-properties source (buffer-string)))
          (gptel-ui-mode -1)
          (should (overlay-buffer native))
          (should (eq (key-binding (kbd "C-c C-c")) #'gptel--accept-tool-calls)))
        (set-marker position nil)))))

(provide 'gptel-ui-integration-test)
;;; gptel-ui-integration-test.el ends here
