;;; gptel-ui-header-test.el --- Approval header contracts -*- lexical-binding: t; -*-

;;; Commentary:
;; Check compact headers, cross-buffer attention and native status restoration.

;;; Code:

(require 'ert)
(require 'gptel-ui)

(defvar gptel-mode)
(defvar gptel-model)
(defvar gptel-use-header-line)
(defvar gptel-tool-call-actions-map)

(ert-deftest gptel-ui-header-compact-preserves-status-and-native-identity ()
  (with-temp-buffer
    (let* ((gptel-mode t)
           (gptel-use-header-line t)
           (gptel-ui-header-details nil)
           (gptel-model 'unneeded-model)
           (native (list "Backend" " Ready" '(:eval "[Prompt: default]"))))
      (setq-local header-line-format native)
      (unwind-protect
          (progn
            (gptel-ui-mode 1)
            (should (= (length header-line-format) 2))
            (should-not (string-match-p "unneeded-model" (gptel-ui--header)))
            (setf (nth 1 header-line-format) " Streaming")
            (gptel-ui-mode -1)
            (should (eq header-line-format native))
            (should (equal (nth 1 native) " Streaming"))
            (should (equal (nth 2 native) '(:eval "[Prompt: default]"))))
        (when gptel-ui-mode (gptel-ui-mode -1))))))

(ert-deftest gptel-ui-header-details-retain-native-controls ()
  (with-temp-buffer
    (let* ((gptel-mode t)
           (gptel-use-header-line t)
           (gptel-ui-header-details t)
           (native (list "Backend" " Ready" '(:eval "Native controls"))))
      (setq-local header-line-format native)
      (unwind-protect
          (progn
            (gptel-ui-mode 1)
            (should (equal (cadar header-line-format) "Backend"))
            (should (eq (nthcdr 2 header-line-format) (nthcdr 2 native)))
            (setf (nth 1 header-line-format) " Waiting")
            (gptel-ui-mode -1)
            (should (eq header-line-format native))
            (should (equal (nth 1 native) " Waiting")))
        (when gptel-ui-mode (gptel-ui-mode -1))))))

(ert-deftest gptel-ui-header-notices-hidden-worker-and-clears-resolved-prompt ()
  (let ((worker (generate-new-buffer " *ui-header-worker*"))
        (gptel-tool-call-actions-map (make-sparse-keymap)))
    (unwind-protect
        (with-temp-buffer
          (gptel-ui-mode 1)
          (unwind-protect
              (progn
                (should-not (string-match-p "approval" (gptel-ui--header)))
                (let ((approval
                       (with-current-buffer worker
                         (insert "Pending tool call")
                         (let ((ov (make-overlay (point-min) (point-max))))
                           (overlay-put ov 'gptel-tool '(pending))
                           (overlay-put ov 'keymap gptel-tool-call-actions-map)
                           ov))))
                  (gptel-ui--approvals-changed)
                  (let* ((header (gptel-ui--header))
                         (badge (string-match "1 approval" header)))
                    (should badge)
                    (should (< badge (string-match "CHAT" header)))
                    (should (eq (get-text-property badge 'face header)
                                'gptel-ui-approval))
                    (should (eq (get-text-property badge 'local-map header)
                                gptel-ui--approval-header-map))
                    (should (string-match-p
                             "ui-header-worker"
                             (get-text-property badge 'help-echo header))))
                  ;; Cancellation or a killed worker must not leave stale badges.
                  (delete-overlay approval)
                  (should-not (string-match-p "approval" (gptel-ui--header)))))
            (gptel-ui-mode -1)))
      (kill-buffer worker))))

(ert-deftest gptel-ui-header-caches-discovery-until-approval-change ()
  (with-temp-buffer
    (gptel-ui-mode 1)
    (unwind-protect
        (let ((scans 0))
          (cl-letf (((symbol-function 'gptel-ui-tools-pending-approvals)
                     (lambda () (cl-incf scans) nil)))
            (gptel-ui--header)
            (gptel-ui--header)
            (should (= scans 1))
            (gptel-ui--approvals-changed)
            (gptel-ui--header)
            (should (= scans 2))))
      (gptel-ui-mode -1))))

(ert-deftest gptel-ui-header-watches-until-last-view-closes ()
  (let ((first (generate-new-buffer " *ui-header-first*"))
        (second (generate-new-buffer " *ui-header-second*")))
    (unwind-protect
        (progn
          (with-current-buffer first (gptel-ui-mode 1))
          (with-current-buffer second (gptel-ui-mode 1))
          (with-current-buffer first (gptel-ui-mode -1))
          (should gptel-ui--watching-approvals)
          (dolist (function gptel-ui--approval-functions)
            (should (advice-member-p #'gptel-ui--approvals-changed function)))
          (kill-buffer second)
          (should-not gptel-ui--watching-approvals)
          (dolist (function gptel-ui--approval-functions)
            (should-not (advice-member-p #'gptel-ui--approvals-changed function)))
          (should-not (memq #'gptel-ui--approvals-changed kill-buffer-hook)))
      (dolist (buffer (list first second))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(provide 'gptel-ui-header-test)
;;; gptel-ui-header-test.el ends here
