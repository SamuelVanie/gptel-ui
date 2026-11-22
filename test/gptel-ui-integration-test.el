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
        ;; The compact header omits native settings but keeps status in slot 1.
        (should (= (length header-line-format) 2))
        (gptel--update-status " Streaming" 'success)
        (should (equal (substring-no-properties (nth 1 header-line-format))
                       " Streaming"))
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
                    :function #'ignore
                    :args '((:name "url" :type string :description "URL"))))
             (position (copy-marker (point)))
             (info (list :position position
                         :backend (gptel--make-backend :name "UI integration")))
             (calls (list (list tool '(:url "https://example.com/preview") #'ignore))))
        ;; This is gptel's actual pending-approval overlay, without execution.
        (gptel--display-tool-calls calls info)
        (goto-char (point-max))
        (insert "\nhttps://example.com/ordinary\n")
        (let* ((native (cl-find-if (lambda (ov) (overlay-get ov 'gptel-tool))
                                   (overlays-in (point-min) (point-max))))
               (source (buffer-string)))
          (should native)
          (goto-char (point-min))
          (search-forward "https://example.com/preview")
          (backward-char 2)
          (should (get-char-property (point) 'gptel-tool))
          (gptel-ui-mode 1)
          (gptel-ui-refresh)
          (should-not (get-char-property (point) 'gptel-ui-link))
          (should (eq (key-binding (kbd "C-c C-c")) #'gptel--accept-tool-calls))
          (should (eq (key-binding (kbd "C-c C-k")) #'gptel--reject-tool-calls))
          (should (eq (key-binding (kbd "C-c C-r")) #'gptel--steer-tool-calls))
          (should (eq (key-binding (kbd "C-c C-i")) #'gptel--inspect-tool-calls))
          (should (eq (overlay-get native 'keymap) gptel-tool-call-actions-map))
          (should (equal-including-properties source (buffer-string)))
          (save-excursion
            (goto-char (point-min))
            (search-forward "https://example.com/ordinary")
            (backward-char 2)
            (should (equal (get-char-property (point) 'gptel-ui-link)
                           "https://example.com/ordinary"))
            (should (eq (key-binding (kbd "RET")) #'gptel-ui-links-open))
            (let (opened)
              (cl-letf (((symbol-function 'browse-url)
                         (lambda (url &rest _) (setq opened url))))
                (call-interactively (key-binding (kbd "RET")))
                (should (equal opened "https://example.com/ordinary")))))
          (gptel-ui-mode -1)
          (should (overlay-buffer native))
          (should (eq (key-binding (kbd "C-c C-c")) #'gptel--accept-tool-calls)))
        (set-marker position nil)))))

(ert-deftest gptel-ui-integration-native-zero-width-preview-preserves-controls ()
  (dolist (case '((org-mode . "C-c C-c") (org-mode . "C-c C-k")
                 (markdown-mode . "C-c C-c") (markdown-mode . "C-c C-k")))
    (gptel-ui-integration-test--chat (car case)
      (setq-local gptel-ui-refresh-delay 0.01)
      (insert "Fetch the URL.\n\n")
      (insert (propertize "Inspect https://example.com/preview\n\n" 'gptel 'response))
      (gptel-ui-mode 1)
      ;; Cancel mode startup's timer so it cannot mask missing lifecycle work.
      (gptel-ui-refresh)
      (should-not gptel-ui--timer)
      (let* ((tool (gptel-make-tool
                    :name "UIIntegrationCustom" :description "Custom preview"
                    :function #'ignore :args nil))
             (position (copy-marker (point-max)))
             (info (list :position position
                         :backend (gptel--make-backend :name "UI integration")))
             preview
             (gptel--tool-preview-alist
              (list (list "UIIntegrationCustom"
                          (lambda (_args _info)
                            (setq preview (make-overlay (point) (point))))
                          #'delete-overlay)))
             (source (buffer-substring-no-properties (point-min) (point-max)))
             (tick (buffer-chars-modified-tick)))
        (goto-char (point-min))
        (search-forward "https://example.com/preview")
        (backward-char 2)
        (should (get-char-property (point) 'gptel-ui-link))
        ;; Native display can be invoked from a different callback buffer.
        (with-temp-buffer
          (gptel--display-tool-calls (list (list tool nil #'ignore)) info))
        (should (= (overlay-start preview) (overlay-end preview)))
        (should (get-char-property (point) 'gptel-tool))
        (should (timerp gptel-ui--timer))
        (sleep-for 0.05)
        (should-not gptel-ui--timer)
        (should-not (get-char-property (point) 'gptel-ui-link))
        (should (eq (key-binding (kbd "C-c C-c")) #'gptel--accept-tool-calls))
        (should (eq (key-binding (kbd "C-c C-k")) #'gptel--reject-tool-calls))
        (let ((modified (buffer-modified-tick)))
          (call-interactively (key-binding (kbd (cdr case))))
          (should (= modified (buffer-modified-tick))))
        (should (timerp gptel-ui--timer))
        (sleep-for 0.05)
        (should-not gptel-ui--timer)
        (should (equal (get-char-property (point) 'gptel-ui-link)
                       "https://example.com/preview"))
        (should (= tick (buffer-chars-modified-tick)))
        (should (equal source (buffer-substring-no-properties (point-min) (point-max))))
        (set-marker position nil)))))

(ert-deftest gptel-ui-integration-copy-response-into-user-draft ()
  (dolist (mode '(org-mode markdown-mode))
    (gptel-ui-integration-test--chat mode
      (setq-local gptel-prompt-prefix-alist (list (cons mode "@user\n"))
                  gptel-response-prefix-alist (list (cons mode "@assistant\n")))
      (insert "@user\nQuestion\n\n@assistant\n")
      (let ((start (point))
            (kill-ring nil)
            (kill-ring-yank-pointer nil)
            (interprogram-cut-function nil)
            (interprogram-paste-function nil))
        (insert (propertize "An answer.\n" 'gptel 'response))
        (let ((end (point)))
          (insert "\n@user\n")
          (gptel-ui-mode 1)
          (copy-region-as-kill start end)
          (goto-char (point-max))
          (let ((draft (point)))
            (yank)
            (should (equal (buffer-substring-no-properties draft (point-max))
                           "An answer.\n"))
            (should-not (text-property-not-all draft (point-max) 'gptel nil))))))))

(ert-deftest gptel-ui-integration-clones-restore-independent-native-headers ()
  (dolist (mode '(org-mode markdown-mode))
    (gptel-ui-integration-test--chat mode
      (insert (propertize "A native response.\n" 'gptel 'response))
      (let ((gptel-highlight-methods '(face)))
        (gptel-highlight-mode 1)
        (gptel-ui-mode 1)
        (gptel--update-status " Source streaming" 'success)
        (let ((source (current-buffer))
              (overlays (copy-sequence gptel-ui--overlays))
              (native-info (nth 2 gptel-ui--native-header))
              (clone (clone-buffer " *ui-integration-clone*" nil)))
          (unwind-protect
              (progn
                (with-current-buffer clone
                  (should gptel-ui-mode)
                  (should (eq (nth 2 gptel-ui--native-header) native-info))
                  (gptel--update-status " Clone ready" 'success)
                  (gptel-ui-mode -1)
                  (should gptel-highlight-mode)
                  (should (equal (substring-no-properties (nth 1 header-line-format))
                                 " Clone ready"))
                  (gptel-mode -1)
                  (should (equal header-line-format "Original header")))
                (should-not gptel-highlight-mode)
                (should (equal (substring-no-properties (nth 1 header-line-format))
                               " Source streaming"))
                (dolist (overlay overlays)
                  (should (eq (overlay-buffer overlay) source))))
            (kill-buffer clone)))))))

(ert-deftest gptel-ui-integration-cross-buffer-native-approval-decisions ()
  (let ((gptel-mode-hook nil)
        (gptel-ui-global-mode nil)
        (gptel-use-header-line t)
        (overriding-terminal-local-map nil)
        (pre-command-hook nil)
        (parent (generate-new-buffer " *ui-approval-parent*"))
        (children (list (generate-new-buffer " *ui-approval-child-a*")
                        (generate-new-buffer " *ui-approval-child-b*")))
        (executed 0)
        (completed 0)
        positions)
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer parent)
          (org-mode)
          (gptel-mode 1)
          (gptel-ui-mode 1)
          (gptel--update-status " Streaming" 'success)
          (dolist (buffer children)
            (with-current-buffer buffer
              ;; Bounded subagent buffers need not have gptel-mode or UI mode.
              (markdown-mode)
              (insert (propertize "Inspecting the project.\n\n" 'gptel 'response))
              (let* ((tool (gptel-make-tool
                            :name "UIIntegrationDecision"
                            :description "Record a fixture approval"
                            :function (lambda () (cl-incf executed) "done")
                            :args nil))
                     (position (copy-marker (point)))
                     (info (list :position position
                                 :backend (gptel--make-backend :name "UI integration"))))
                (push position positions)
                (gptel--display-tool-calls
                 (list (list tool nil (lambda (_) (cl-incf completed)))) info))))
          (should (= (length (gptel-ui-tools-pending-approvals)) 2))
          (should (string-match-p "2 approvals" (gptel-ui--header)))
          (should (equal (substring-no-properties (nth 1 header-line-format))
                         " Streaming"))
          (should (eq (key-binding (kbd "C-c ]")) #'gptel-ui-next-approval))
          (call-interactively #'gptel-ui-next-approval)
          (should (memq (current-buffer) children))
          (should (get-char-property (point) 'gptel-tool))
          (should (eq (key-binding (kbd "C-c C-c")) #'gptel--accept-tool-calls))
          (call-interactively #'gptel--accept-tool-calls)
          (should (= executed 1))
          (should (= completed 1))
          (should (= (length (gptel-ui-tools-pending-approvals)) 1))
          (with-current-buffer parent
            (should (string-match-p "1 approval" (gptel-ui--header))))
          (call-interactively #'gptel-ui-next-approval)
          (should (get-char-property (point) 'gptel-tool))
          (should (eq (key-binding (kbd "C-c C-k")) #'gptel--reject-tool-calls))
          (call-interactively #'gptel--reject-tool-calls)
          (should (= executed 1))
          (should (= completed 1))
          (should-not (gptel-ui-tools-pending-approvals))
          (with-current-buffer parent
            (should-not (string-match-p "approval" (gptel-ui--header)))))
      (dolist (position positions) (set-marker position nil))
      (dolist (buffer (cons parent children))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when gptel-ui-mode (gptel-ui-mode -1))
            (when gptel-mode (gptel-mode -1)))
          (kill-buffer buffer))))))

(provide 'gptel-ui-integration-test)
;;; gptel-ui-integration-test.el ends here
