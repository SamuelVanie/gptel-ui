;;; gptel-ui-integration-test.el --- Native gptel integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Run separately with real gptel and markdown-mode on load-path.  These tests
;; exercise native chat rendering, commands and pending tool controls.
;; The streaming test uses an in-process mock transport.

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

(defun gptel-ui-integration-test--approval-navigation (keys command &optional inspect)
  "Run native approval COMMAND via KEYS, optionally from INSPECT, then cycle."
  (let ((gptel-mode-hook nil)
        (gptel--known-tools (copy-tree gptel--known-tools))
        (gptel-ui-global-mode nil)
        (gptel-display-buffer-action '(display-buffer-same-window))
        (gptel-ui-refresh-delay 3600)
        (overriding-terminal-local-map nil)
        (pre-command-hook nil)
        (parent (generate-new-buffer " *ui-navigation-parent*"))
        (workers (list (generate-new-buffer " *ui-navigation-worker-a*")
                       (generate-new-buffer " *ui-navigation-worker-b*")))
        (original-set-transient-map (symbol-function 'set-transient-map))
        (installed 0)
        (executed 0)
        (completed 0)
        remembered exit-function)
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'gptel-request)
                     (lambda (&rest _) (ert-fail "Unexpected model request")))
                    ((symbol-function 'set-transient-map)
                     (lambda (&rest args)
                       (cl-incf installed)
                       (setq exit-function (apply original-set-transient-map args)))))
            (switch-to-buffer parent)
            (text-mode)
            (gptel-mode 1)
            (gptel-ui-mode 1)
            (dolist (worker workers)
              (with-current-buffer worker
                (text-mode)
                (insert (propertize "Inspecting the project.\n\n" 'gptel 'response))
                (let* ((tool (gptel-make-tool
                              :name "UIApprovalNavigation"
                              :description "Record a fixture approval"
                              :function (lambda ()
                                          (cl-incf executed)
                                          (setq remembered
                                                (bound-and-true-p
                                                 gptel-agent-access--remember))
                                          "done")
                              :args nil))
                       (info (list :position (point-marker)
                                   :backend (gptel--make-backend :name "UI integration")
                                   :tool-use '((:name "UIApprovalNavigation" :args nil)))))
                  (gptel--display-tool-calls
                   (list (list tool nil (lambda (_) (cl-incf completed)))) info))))
            ;; Use the command loop: calling commands directly skips the
            ;; transient map's pre-command lifetime check.
            (execute-kbd-macro (kbd "C-c ]"))
            (should (eq (current-buffer) (car workers)))
            (should-not gptel-ui-mode)
            (should (= executed 0))
            (when inspect
              (should (eq (key-binding (kbd "C-c C-i")) #'gptel--inspect-tool-calls))
              (execute-kbd-macro (kbd "C-c C-i"))
              (should (equal (buffer-name) "*gptel-tool-calls*"))
              ;; The inspection copies must not enter the approval queue.
              (should (= (length (gptel-ui-tools-pending-approvals)) 2)))
            (should (eq (key-binding (kbd keys)) command))
            (if (memq command '(gptel--steer-tool-calls gptel--inspect-steer-tool-calls))
                ;; Exercise real minibuffer input inside the native command.
                (let ((noninteractive nil))
                  (execute-kbd-macro (vconcat (kbd keys) "Try again" (kbd "RET"))))
              (execute-kbd-macro (kbd keys)))
            (should (eq (current-buffer) (car workers)))
            (should (= executed (if (memq command '(gptel--inspect-accept-tool-calls
                                                    gptel-agent-allow-outside-access))
                                    1 0)))
            (should (= completed (if (memq command '(gptel--inspect-quit-tool-calls
                                                     gptel--inspect-reject-tool-calls))
                                     0 1)))
            (when (eq command 'gptel-agent-allow-outside-access)
              (should remembered))
            (should (= (length (gptel-ui-tools-pending-approvals))
                       (if (eq command 'gptel--inspect-quit-tool-calls) 2 1)))
            (should (eq (key-binding (kbd "C-c ]")) #'gptel-ui-next-approval))
            (should (eq (key-binding (kbd "C-c [")) #'gptel-ui-previous-approval))
            (execute-kbd-macro (kbd "C-c ]"))
            (should (eq (current-buffer) (cadr workers)))
            (execute-kbd-macro (kbd "C-c ["))
            (should (eq (current-buffer)
                        (if (eq command 'gptel--inspect-quit-tool-calls)
                            (car workers) (cadr workers))))
            (should (= installed 1))
            (should (eq (key-binding (kbd "C-c C-c")) #'gptel--accept-tool-calls))
            (should (eq (key-binding (kbd "C-c C-k")) #'gptel--reject-tool-calls))
            ;; Ordinary movement still ends the temporary navigation session.
            (execute-kbd-macro (kbd "C-f"))
            (should-not (eq (key-binding (kbd "C-c ]")) #'gptel-ui-next-approval))
            (should-not (eq (key-binding (kbd "C-c [")) #'gptel-ui-previous-approval))))
      (when exit-function (funcall exit-function))
      (dolist (buffer (append (list parent (get-buffer "*gptel-tool-calls*")) workers))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when gptel-ui-mode (gptel-ui-mode -1))
            (when gptel-mode (gptel-mode -1)))
          (kill-buffer buffer))))))

(ert-deftest gptel-ui-integration-navigation-survives-inspection-actions ()
  (dolist (action '(("C-c C-i" gptel--inspect-quit-tool-calls)
                    ("C-c C-c" gptel--inspect-accept-tool-calls)
                    ("C-c C-k" gptel--inspect-reject-tool-calls)
                    ("C-c C-r" gptel--inspect-steer-tool-calls)))
    (ert-info ((format "Inspection action %s" (cadr action)))
      (gptel-ui-integration-test--approval-navigation (car action) (cadr action) t))))

(ert-deftest gptel-ui-integration-navigation-survives-steering-input ()
  (gptel-ui-integration-test--approval-navigation "C-c C-r" #'gptel--steer-tool-calls))

(ert-deftest gptel-ui-integration-navigation-survives-outside-access ()
  ;; gptel-agent is optional; the core integration suite needs only gptel.
  (skip-unless (require 'gptel-agent-access nil t))
  (gptel-ui-integration-test--approval-navigation
   "C-c C-a" #'gptel-agent-allow-outside-access))

(ert-deftest gptel-ui-integration-native-tool-result-nested-fences ()
  (dolist (mode '(markdown-mode org-mode))
    (gptel-ui-integration-test--chat mode
      (let* ((gptel-include-tool-results t)
             (tool (gptel-make-tool :name "UIIntegrationRead"
                                    :description "Read a fixture"
                                    :function #'ignore :args nil))
             (info (list :buffer (current-buffer) :position (point-marker)
                         :callback #'gptel--insert-response
                         :tool-use '((:name "UIIntegrationRead" :id "read-1")))))
        ;; Use native insertion, including its wrappers, role properties and
        ;; Org escaping.  No tool function or request is executed.
        (gptel--display-tool-results
         (list (list tool '("README.md")
                     "Example:\n```elisp\n(message \"hello\")\n```\n#+end_tool\nMore file content"))
         info)
        (goto-char (point-max))
        (let ((tool-beg (save-excursion
                          (goto-char (point-min))
                          (skip-chars-forward "\n")
                          (point)))
              (tool-end (1- (point))) (answer (point)))
          (insert (propertize
                   (if (eq mode 'org-mode)
                       "\n* Summary\n[[https://example.com/result][Result]]\n"
                     "\n# Summary\n[Result](https://example.com/result)\n")
                   'gptel 'response))
          (let ((source (buffer-string))
                (native-overlays
                 (mapcar (lambda (ov) (list ov (overlay-start ov) (overlay-end ov)
                                           (overlay-properties ov)))
                         (overlays-in (point-min) (point-max)))))
            (gptel-ui-mode 1)
            (should (equal gptel-ui--excluded (list (cons tool-beg tool-end))))
            (should (= 1 (length gptel-ui--tool-starts)))
            (should-not (gptel-ui--excluded-p answer (point-max)))
            (should-not (cl-some (lambda (ov) (eq (overlay-get ov 'face) 'gptel-ui-code))
                                gptel-ui--overlays))
            (should (= 1 (cl-count-if
                          (lambda (ov) (eq (overlay-get ov 'face) 'gptel-ui-heading))
                          gptel-ui--overlays)))
            (should (= 1 (cl-count-if (lambda (ov) (overlay-get ov 'gptel-ui-link))
                                    gptel-ui--overlays)))
            (should (equal-including-properties source (buffer-string)))
            (dolist (entry native-overlays)
              (let ((ov (car entry)))
                (should (equal (cdr entry)
                               (list (overlay-start ov) (overlay-end ov)
                                     (overlay-properties ov))))))))))))

(ert-deftest gptel-ui-integration-consecutive-tool-results-before-assistant ()
  (dolist (prefix '("[Result](https://example.com/result)\n"
                    "````text\n```\n[Literal](https://example.com/literal)\n````\n"))
    (gptel-ui-integration-test--chat #'markdown-mode
      (let* ((gptel-include-tool-results t)
             (tool (gptel-make-tool :name "UIIntegrationRead"
                                    :description "Read a fixture"
                                    :function #'ignore :args nil))
             (info (list :buffer (current-buffer) :position (point-marker)
                         :callback #'gptel--insert-response
                         :tool-use '((:name "UIIntegrationRead" :id "read-1"))))
             (code (string-prefix-p "````" prefix)))
        (gptel--display-tool-results
         (list (list tool '("first.md") "First:\n```elisp\n(+ 1 2)\n```\nTail")
               (list tool '("second.md") "Second:\n````text\n```\n````\nTail"))
         info)
        (goto-char (point-max))
        (let ((answer (point)))
          (insert (propertize (concat prefix "\n# Summary\n"
                                     (when code "[Result](https://example.com/result)\n"))
                              'gptel 'response))
          (gptel-ui-mode 1)
          (should (= 2 (length gptel-ui--tool-starts)))
          (should (= (if code 3 2) (length gptel-ui--excluded)))
          (should (= (if code 1 0)
                     (cl-count-if (lambda (ov) (eq (overlay-get ov 'face) 'gptel-ui-code))
                                  gptel-ui--overlays)))
          (should (= 1 (cl-count-if
                        (lambda (ov) (and (>= (overlay-start ov) answer)
                                         (overlay-get ov 'gptel-ui-link)))
                        gptel-ui--overlays)))
          (should (= 1 (cl-count-if
                        (lambda (ov) (and (>= (overlay-start ov) answer)
                                         (eq (overlay-get ov 'face) 'gptel-ui-heading)))
                        gptel-ui--overlays))))))))

(ert-deftest gptel-ui-integration-navigate-after-aborted-stream ()
  (require 'gptel-openai)
  (with-temp-buffer
    (markdown-mode)
    (gptel-mode 1)
    (let* ((gptel--known-backends nil)
           (gptel-backend (gptel-make-openai "UI fixture" :key "unused" :stream t))
           (gptel-model 'gpt-4o)
           (gptel-stream t)
           (gptel-use-curl t)
           (gptel-prompt-transform-functions nil)
           (gptel--request-alist nil)
           (gptel-prompt-prefix-alist '((markdown-mode . "### ")))
           (gptel-response-prefix-alist '((markdown-mode . "")))
           machine)
      ;; Capture the request at the transport boundary.  The stream callback,
      ;; abort command, role properties and follow-up prompt are all native.
      (cl-letf (((symbol-function 'gptel-curl-get-response)
                 (lambda (fsm)
                   (setq machine fsm)
                   (plist-put (gptel-fsm-info fsm) :callback
                              #'gptel-curl--stream-insert-response)
                   (push (cons 'fixture (cons fsm #'ignore)) gptel--request-alist))))
        (cl-flet ((send-chunk (text)
                    (gptel-send)
                    (gptel--fsm-transition machine)
                    (let ((info (gptel-fsm-info machine)))
                      (funcall (plist-get info :callback) text info))))
          (insert (gptel-prompt-prefix-string) "Show an example.")
          (gptel-ui-mode 1)
          (send-chunk "```elisp\n(+ 1")
          (gptel-abort (current-buffer))
          (should (eq (gptel-fsm-state machine) 'ABRT))
          (goto-char (point-max))
          (insert "\n")
          (let ((follow-up (point)))
            (insert (gptel-prompt-prefix-string) "Show the docs.")
            (send-chunk "[Manual](https://example.org/manual)")
            (gptel--fsm-transition machine)
            (should (eq (gptel-fsm-state machine) 'DONE))
            (gptel-ui-refresh)
            (goto-char (point-min))
            (search-forward "(+ 1")
            (call-interactively (key-binding (kbd "C-c C-n")))
            (should (= (point) follow-up))
            (call-interactively (key-binding (kbd "C-c C-n")))
            (should (equal (get-char-property (point) 'gptel-ui-link)
                           "https://example.org/manual"))
            (call-interactively (key-binding (kbd "C-c C-p")))
            (should (= (point) follow-up))))))))

(provide 'gptel-ui-integration-test)
;;; gptel-ui-integration-test.el ends here
