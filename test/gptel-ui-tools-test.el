;;; gptel-ui-tools-test.el --- Tool activity presentation -*- lexical-binding: t; -*-

;;; Commentary:
;; Native fold, approval, and transcript boundaries of the tool renderer.

;;; Code:

(require 'ert)
(require 'org)
(require 'gptel-ui-tools)

(defvar gptel-tool-call-actions-map)

(defun gptel-ui-tools-test--insert (&optional incomplete)
  "Insert a native Org tool result, returning renderer arguments.
INCOMPLETE omits the closing fence."
  (let ((beg (point)) opening-end body-beg close-beg)
    (insert "#+begin_tool (Bash \"pwd\")")
    (setq opening-end (point))
    (insert "\n")
    (setq body-beg (point))
    (insert (propertize "(:name \"Bash\" :args (\"pwd\"))\n\n/work/project\n"
                        'gptel '(tool . "call-1")))
    (unless incomplete
      (setq close-beg (point))
      (insert "#+end_tool"))
    (list beg opening-end body-beg close-beg (point) "(Bash \"pwd\")")))

(ert-deftest gptel-ui-tools-preserve-transcript-and-role-properties ()
  (with-temp-buffer
    (org-mode)
    (let* ((arguments (gptel-ui-tools-test--insert))
           (before (buffer-string)) (undo buffer-undo-list) (point (point)))
      (set-buffer-modified-p nil)
      (let* ((overlays (apply #'gptel-ui-tools-render-block arguments))
             (header (cl-find-if (lambda (ov) (overlay-get ov 'gptel-ui-tool-header)) overlays)))
        (should (equal-including-properties before (buffer-string)))
        (should-not (buffer-modified-p))
        (should (equal undo buffer-undo-list))
        (should (= point (point)))
        (should header)
        (should (overlay-get header 'gptel-ui-block-opening))
        (should (string-match-p "TOOL  ·  Bash.*pwd" (overlay-get header 'display)))
        (should-not (string-match-p "success\|complete\|passed" (overlay-get header 'display)))
        (should (cl-some (lambda (ov) (string-match-p "INPUT\npwd"
                                                     (or (overlay-get ov 'display) ""))) overlays))
        (should (cl-some (lambda (ov) (equal (substring-no-properties
                                            (or (overlay-get ov 'before-string) ""))
                                           "OUTPUT\n")) overlays))
        (mapc #'delete-overlay overlays)
        (should (equal-including-properties before (buffer-string)))))))

(ert-deftest gptel-ui-tools-retain-native-folds-and-unhide-only-header ()
  (with-temp-buffer
    (org-mode)
    (let* ((arguments (gptel-ui-tools-test--insert))
           (beg (nth 0 arguments)) (body-beg (nth 2 arguments)))
      (goto-char beg)
      (org-cycle)
      (should (invisible-p body-beg))
      (add-to-invisibility-spec 'org-modern)
      (put-text-property beg (+ beg 12) 'invisible 'org-modern)
      (let ((before (buffer-string))
            (overlays (apply #'gptel-ui-tools-render-block arguments)))
        (should-not (invisible-p beg))
        (should (invisible-p body-beg))
        (should (equal-including-properties before (buffer-string)))
        (mapc #'delete-overlay overlays)
        (should (invisible-p beg))
        (should (invisible-p body-beg))))))

(ert-deftest gptel-ui-tools-preserve-spanning-native-approval-keymap ()
  (with-temp-buffer
    (org-mode)
    (let* ((arguments (gptel-ui-tools-test--insert))
           (native (make-overlay (point-min) (point-max)))
           (map (make-sparse-keymap)))
      (define-key map (kbd "C-c C-c") #'ignore)
      (overlay-put native 'gptel-tool '(pending))
      (overlay-put native 'keymap map)
      (let* ((overlays (apply #'gptel-ui-tools-render-block arguments))
             (header (cl-find-if (lambda (ov) (overlay-get ov 'gptel-ui-tool-header)) overlays)))
        (goto-char (point-min))
        (should (eq (key-binding (kbd "C-c C-c")) #'ignore))
        (should-not (overlay-get header 'keymap))
        (should (eq (overlay-get native 'keymap) map))
        (mapc #'delete-overlay overlays)
        (should (overlay-buffer native))))))

(ert-deftest gptel-ui-tools-ignore-untyped-examples-and-pending-text ()
  (with-temp-buffer
    (org-mode)
    (let ((arguments (gptel-ui-tools-test--insert)))
      (remove-text-properties (point-min) (point-max) '(gptel nil))
      (should-not (apply #'gptel-ui-tools-render-block arguments)))))

(ert-deftest gptel-ui-tools-incomplete-block-has-no-fold-action ()
  (with-temp-buffer
    (org-mode)
    (let* ((arguments (gptel-ui-tools-test--insert t))
           (overlays (apply #'gptel-ui-tools-render-block arguments))
           (header (cl-find-if (lambda (ov) (overlay-get ov 'gptel-ui-tool-header)) overlays)))
      (should header)
      (should-not (overlay-get header 'keymap)))))

(ert-deftest gptel-ui-tools-use-native-org-fold-toggle ()
  (with-temp-buffer
    (org-mode)
    (let* ((arguments (gptel-ui-tools-test--insert))
           (body-beg (nth 2 arguments))
           (overlays (apply #'gptel-ui-tools-render-block arguments)))
      (goto-char (point-min))
      (should-not (invisible-p body-beg))
      (gptel-ui-tools-toggle)
      (should (invisible-p body-beg))
      (gptel-ui-tools-toggle)
      (should-not (invisible-p body-beg))
      (mapc #'delete-overlay overlays))))

(ert-deftest gptel-ui-tools-read-metadata-without-evaluation ()
  (with-temp-buffer
    (insert "(:name \"Bash\" :args ((progn (error \"must not run\"))))")
    (let ((metadata (gptel-ui-tools--metadata (point-min) (point-max))))
      (should (equal (plist-get (car metadata) :name) "Bash"))
      (should (string-match-p "must not run"
                              (gptel-ui-tools--input (plist-get (car metadata) :args)))))))

(ert-deftest gptel-ui-tools-do-not-hide-headers-with-default-invisibility ()
  (with-temp-buffer
    (text-mode)
    (let ((arguments (gptel-ui-tools-test--insert)))
      (setq-local buffer-invisibility-spec t)
      (apply #'gptel-ui-tools-render-block arguments)
      (should-not (invisible-p (point-min))))))

(defun gptel-ui-tools-test--approval (buffer start end &optional prompt-start)
  "Create a fake native approval in BUFFER from START to END.
When non-nil, PROMPT-START marks the visible prompt within the dispatch area."
  (with-current-buffer buffer
    (let ((overlay (make-overlay start end)))
      (overlay-put overlay 'info (list :position (copy-marker start)))
      (overlay-put overlay 'gptel-tool '(pending))
      (overlay-put overlay 'keymap gptel-tool-call-actions-map)
      (when prompt-start
        (overlay-put overlay 'prompt (list (make-overlay prompt-start end))))
      overlay)))

(ert-deftest gptel-ui-tools-find-hidden-and-narrowed-approvals-only ()
  (let ((gptel-tool-call-actions-map (make-sparse-keymap))
        (first (generate-new-buffer " *ui-approval-a*"))
        (second (generate-new-buffer " *ui-approval-b*")))
    (unwind-protect
        (let (early late other)
          (with-current-buffer first
            (insert (make-string 100 ?x))
            (setq late (gptel-ui-tools-test--approval first 60 90 75)
                  early (gptel-ui-tools-test--approval first 10 40 25))
            (let ((inspection (make-overlay 45 55)))
              (overlay-put inspection 'gptel-tool '(inspection-copy)))
            (let ((stale (gptel-ui-tools-test--approval first 91 99)))
              (delete-overlay stale))
            (narrow-to-region 41 59))
          (with-current-buffer second
            (insert "pending")
            (setq other (gptel-ui-tools-test--approval second 1 (point-max))))
          (should (equal (gptel-ui-tools-pending-approvals)
                         (list early late other)))
          ;; Reading the queue must preserve the buffer's restriction.
          (with-current-buffer first
            (should (= (point-min) 41))
            (should (= (point-max) 59))))
      (kill-buffer first)
      (kill-buffer second))))

(ert-deftest gptel-ui-tools-discover-approval-in-its-request-view ()
  (let ((gptel-tool-call-actions-map (make-sparse-keymap)))
    (with-temp-buffer
      (insert "shared transcript")
      (let* ((base (current-buffer))
             (original (gptel-ui-tools-test--approval base 1 6))
             ;; A clone can predate loading Atelier and its clone hooks.
             (view (let ((clone-indirect-buffer-hook nil))
                     (clone-indirect-buffer " *ui-existing-approval-view*" nil))))
        (unwind-protect
            (with-current-buffer view
              (should (get-char-property (point-min) 'gptel-tool))
              (let ((own (gptel-ui-tools-test--approval view 7 (point-max))))
                (let ((pending (gptel-ui-tools-pending-approvals)))
                  (should (= (length pending) 2))
                  (should (memq original pending))
                  (should (memq own pending)))
                (delete-overlay original)
                (should (equal (gptel-ui-tools-pending-approvals) (list own)))
                (set-marker (plist-get (overlay-get own 'info) :position) nil)
                (should-not (gptel-ui-tools-pending-approvals))))
          (kill-buffer view))))))

(ert-deftest gptel-ui-tools-legacy-dispatch-uses-original-prompt-buffer ()
  (let ((gptel-tool-call-actions-map (make-sparse-keymap)))
    (with-temp-buffer
      (insert "legacy prompt")
      (let* ((prompt (make-overlay 4 (point-max)))
             (dispatch (gptel-ui-tools-test--approval (current-buffer) 1 (point-max))))
        ;; gptel 0.9.9.4 used a single prompt overlay and no info plist.
        (overlay-put dispatch 'info nil)
        (overlay-put dispatch 'prompt prompt)
        (let ((view (let ((clone-indirect-buffer-hook nil))
                      (clone-indirect-buffer " *ui-legacy-approval-view*" nil))))
          (unwind-protect
              (progn
                (should (equal (gptel-ui-tools-pending-approvals) (list dispatch)))
                (should (= (gptel-ui-tools--approval-position dispatch) 4)))
            (kill-buffer view)))))))

(ert-deftest gptel-ui-tools-cycle-prompts-across-buffers-and-wrap ()
  (let ((gptel-tool-call-actions-map (make-sparse-keymap))
        (first (generate-new-buffer " *ui-approval-a*"))
        (second (generate-new-buffer " *ui-approval-b*")))
    (unwind-protect
        (save-window-excursion
          (let (early late other)
            (with-current-buffer first
              (insert (make-string 100 ?x))
              (setq early (gptel-ui-tools-test--approval first 10 40 25)
                    late (gptel-ui-tools-test--approval first 50 90 75)))
            (with-current-buffer second
              (insert "pending")
              (setq other (gptel-ui-tools-test--approval second 1 (point-max))))
            ;; Avoid installing an interactive transient map in this unit test.
            (cl-letf (((symbol-function 'set-transient-map) #'ignore))
              (switch-to-buffer first)
              (goto-char 1)
              (should (eq (gptel-ui-next-approval) early))
              (should (= (point) 25))
              (should (eq (gptel-ui-next-approval) late))
              (should (= (point) 75))
              (should (eq (gptel-ui-next-approval) other))
              (should (eq (current-buffer) second))
              (should (eq (gptel-ui-next-approval) early))
              (should (eq (gptel-ui-previous-approval) other))
              (delete-overlay late)
              (should (eq (gptel-ui-previous-approval) early))
              (delete-overlay other)
              (should (eq (gptel-ui-next-approval) early))
              (delete-overlay early)
              (should-error (gptel-ui-next-approval) :type 'user-error))))
      (kill-buffer first)
      (kill-buffer second))))

(ert-deftest gptel-ui-tools-navigation-reveals-narrowed-native-prompt ()
  (let ((gptel-tool-call-actions-map (make-sparse-keymap))
        (buffer (generate-new-buffer " *ui-approval-native*")))
    (define-key gptel-tool-call-actions-map (kbd "C-c C-c") #'ignore)
    (define-key gptel-tool-call-actions-map (kbd "C-c C-k") #'ignore)
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (insert (make-string 100 ?x))
          (let ((approval (gptel-ui-tools-test--approval buffer 10 90 75))
                (text (buffer-string))
                transient-map keep-predicate)
            (narrow-to-region 1 9)
            (cl-letf (((symbol-function 'set-transient-map)
                       (lambda (map predicate &rest _)
                         (setq transient-map map keep-predicate predicate))))
              (should (eq (gptel-ui-next-approval) approval)))
            (should (= (point) 75))
            (should-not (buffer-narrowed-p))
            (should (equal-including-properties text (buffer-string)))
            (should (eq (overlay-get approval 'keymap) gptel-tool-call-actions-map))
            ;; The temporary map must let the original approval shortcuts run.
            (let ((overriding-terminal-local-map transient-map))
              (should (eq (key-binding (kbd "C-c ]")) #'gptel-ui-next-approval))
              (should (eq (key-binding (kbd "C-c [")) #'gptel-ui-previous-approval))
              (should (eq (key-binding (kbd "C-c C-c")) #'ignore))
              (should (eq (key-binding (kbd "C-c C-k")) #'ignore)))
            (let ((this-command 'gptel--accept-tool-calls))
              (should (funcall keep-predicate)))
            (let ((this-command 'forward-char))
              (should-not (funcall keep-predicate)))))
      (kill-buffer buffer))))

(ert-deftest gptel-ui-tools-zero-width-preview-keeps-native-keys-active ()
  (let ((gptel-tool-call-actions-map (make-sparse-keymap)))
    (with-temp-buffer
      (insert "response")
      (let ((approval (gptel-ui-tools-test--approval
                       (current-buffer) 1 (point-max) (point-max))))
        (should (= (gptel-ui-tools--approval-position approval) (1- (point-max))))
        (should (eq (get-char-property (gptel-ui-tools--approval-position approval)
                                       'keymap)
                    gptel-tool-call-actions-map))))))

(ert-deftest gptel-ui-tools-transient-navigation-does-not-stack-or-shadow-actions ()
  (let ((gptel-tool-call-actions-map (make-sparse-keymap))
        (buffer (generate-new-buffer " *ui-approval-transient*"))
        (overriding-terminal-local-map nil)
        (pre-command-hook nil)
        (original-set-transient-map (symbol-function 'set-transient-map))
        (install-count 0)
        exit-function)
    (define-key gptel-tool-call-actions-map (kbd "C-c C-c") #'ignore)
    (define-key gptel-tool-call-actions-map (kbd "C-c C-k") #'ignore)
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (insert "response\npending\n")
          (gptel-ui-tools-test--approval buffer 1 (point-max) 10)
          (cl-letf (((symbol-function 'set-transient-map)
                     (lambda (&rest arguments)
                       (cl-incf install-count)
                       (setq exit-function (apply original-set-transient-map arguments)))))
            (gptel-ui-next-approval)
            (gptel-ui-next-approval)
            (gptel-ui-previous-approval)
            (should (= install-count 1))
            (should (eq (key-binding (kbd "C-c ]")) #'gptel-ui-next-approval))
            (should (eq (key-binding (kbd "C-c C-c")) #'ignore))
            (should (eq (key-binding (kbd "C-c C-k")) #'ignore))))
      (when exit-function (funcall exit-function))
      (kill-buffer buffer))))

(ert-deftest gptel-ui-tools-navigation-ends-before-unrelated-minibuffer ()
  (let ((gptel-tool-call-actions-map (make-sparse-keymap))
        (buffer (generate-new-buffer " *ui-approval-minibuffer*"))
        (overriding-terminal-local-map nil)
        (pre-command-hook nil)
        (original-set-transient-map (symbol-function 'set-transient-map))
        prompted exit-function)
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (text-mode)
          (insert "response\npending\n")
          (gptel-ui-tools-test--approval buffer 1 (point-max) 10)
          (cl-letf (((symbol-function 'set-transient-map)
                     (lambda (&rest args)
                       (setq exit-function (apply original-set-transient-map args)))))
            (gptel-ui-next-approval))
          (should (eq (key-binding (kbd "C-c ]")) #'gptel-ui-next-approval))
          (let ((noninteractive nil)
                (minibuffer-setup-hook
                 (list (lambda ()
                         (setq prompted t)
                         (should-not (eq (key-binding (kbd "C-c ]"))
                                         #'gptel-ui-next-approval))))))
            (execute-kbd-macro (vconcat (kbd "M-:") "nil" (kbd "RET"))))
          (should prompted)
          (should-not (eq (key-binding (kbd "C-c ]")) #'gptel-ui-next-approval)))
      (when exit-function (funcall exit-function))
      (kill-buffer buffer))))

(provide 'gptel-ui-tools-test)
;;; gptel-ui-tools-test.el ends here
