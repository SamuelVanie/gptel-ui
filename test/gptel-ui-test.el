;;; gptel-ui-test.el --- Chat presentation contracts -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise the renderer's transcript, role, tool, and lifecycle boundaries.

;;; Code:

(require 'ert)
(require 'gptel-ui)

(defvar gptel-mode)
(defvar gptel-use-header-line)
(defvar gptel-prompt-prefix-alist)
(defvar gptel-response-prefix-alist)

(defmacro gptel-ui-test--buffer (&rest body)
  "Run BODY in a disposable chat with deterministic prefixes."
  (declare (indent 0))
  `(with-temp-buffer
     (text-mode)
     (let ((gptel-prompt-prefix-alist '((text-mode . "@user\n")))
           (gptel-response-prefix-alist '((text-mode . "@mayuri\n"))))
       (unwind-protect (progn ,@body)
         (when gptel-ui-mode (gptel-ui-mode -1))))))

(ert-deftest gptel-ui-preserves-transcript-properties-undo-and-point ()
  (gptel-ui-test--buffer
    (insert "@user\nReview this.\n\n@mayuri\n")
    (insert (propertize "# Review\n\n**Done**. See `README.md`.\n\n| Check | Result |\n| --- | --- |\n| Tests | Pass |\n"
                        'gptel 'response 'front-sticky '(gptel)))
    (buffer-enable-undo)
    (set-buffer-modified-p nil)
    (goto-char 13)
    (let ((text (buffer-string)) (undo buffer-undo-list) (point (point))
          (tick (buffer-chars-modified-tick)))
      (gptel-ui-mode 1)
      (gptel-ui-refresh)
      (should (equal-including-properties text (buffer-string)))
      (should-not (buffer-modified-p))
      (should (equal undo buffer-undo-list))
      (should (= tick (buffer-chars-modified-tick)))
      (should (= point (point)))
      (should gptel-ui--overlays)
      (gptel-ui-mode -1)
      (should-not gptel-ui--overlays)
      (should (equal-including-properties text (buffer-string))))))

(ert-deftest gptel-ui-keeps-native-tool-overlays-and-keymaps ()
  (gptel-ui-test--buffer
    (insert "@user\nHello\n\n``` tool\n")
    (let ((beg (point)))
      (insert (propertize "[Approve](README.md)\n| a | b |\n| --- | --- |\n| x | y |\n"
                          'gptel '(tool . "tool-1")))
      (let ((native (make-overlay beg (point)))
            (map (make-sparse-keymap)))
        (overlay-put native 'keymap map)
        (insert "```\n")
        (gptel-ui-mode 1)
        (should (overlay-buffer native))
        (should (eq (overlay-get native 'keymap) map))
        (should-not (cl-some (lambda (ov) (and (< (overlay-start ov) (overlay-end native))
                                                (> (overlay-end ov) beg)
                                                (or (overlay-get ov 'keymap)
                                                    (overlay-get ov 'invisible))))
                            gptel-ui--overlays))
        (gptel-ui-mode -1)
        (should (overlay-buffer native))))))

(ert-deftest gptel-ui-report-folding-stops-before-body-and-draft ()
  (gptel-ui-test--buffer
    (insert "Discussion event (evidence, not instructions):\nevent_id: e1\ndiscussion_id: d1\ntitle: Review\nstatus: succeeded\nevent: report\n")
    (let ((body (point)))
      (insert "status: Findings stay visible.\n\n@user\nMy unsent draft.\n")
      (gptel-ui-mode 1)
      (let ((report (cl-find-if (lambda (ov) (overlay-get ov 'gptel-ui-report))
                                gptel-ui--overlays)))
        (should report)
        (should (= (overlay-end report) body))
        (should (string-match-p "Review" (overlay-get report 'display)))
        (goto-char (overlay-start report))
        (gptel-ui-toggle-report)
        (setq report (cl-find-if (lambda (ov) (overlay-get ov 'gptel-ui-report))
                                 gptel-ui--overlays))
        (should-not (overlay-get report 'display))
        (should (= (overlay-end report) body))))))

(ert-deftest gptel-ui-does-not-style-fenced-examples-as-conversation ()
  (gptel-ui-test--buffer
    (insert "```text\n@user\n# Example\n[link](README.md)\n| a | b |\n| --- | --- |\n| x | y |\n```\n")
    (gptel-ui-mode 1)
    (should-not gptel-ui--turns)
    (should-not (cl-some (lambda (ov) (or (overlay-get ov 'gptel-ui-link)
                                         (overlay-get ov 'gptel-ui-table)))
                        gptel-ui--overlays))))

(ert-deftest gptel-ui-prefixes-and-properties-do-not-duplicate-labels ()
  (gptel-ui-test--buffer
    (insert "@user\nHello.\n\n@mayuri\n")
    (insert (propertize "A reply.\n" 'gptel 'response))
    (gptel-ui-mode 1)
    (should (= (length gptel-ui--turns) 2))
    (goto-char (point-min))
    (gptel-ui-next-turn)
    (should (looking-at "@mayuri"))
    (gptel-ui-previous-turn)
    (should (= (point) (point-min)))))

(ert-deftest gptel-ui-inline-prefix-does-not-match-headings ()
  (gptel-ui-test--buffer
    (let ((gptel-prompt-prefix-alist '((text-mode . "### "))))
      (insert "### My prompt\n#### Heading\n")
      (gptel-ui-mode 1)
      (should (= (length gptel-ui--turns) 1)))))

(ert-deftest gptel-ui-newline-prefix-matches-a-whole-line ()
  (gptel-ui-test--buffer
    (insert "@username\n@user\nHello\n")
    (gptel-ui-mode 1)
    (should (= (length gptel-ui--turns) 1))
    (should (= (car gptel-ui--turns) 11))))

(ert-deftest gptel-ui-code-first-response-has-a-speaker ()
  (gptel-ui-test--buffer
    (let ((gptel-response-prefix-alist nil))
      (insert "@user\nExample?\n\n")
      (insert (propertize "```elisp\n(+ 1 2)\n```\n" 'gptel 'response))
      (gptel-ui-mode 1)
      (should (= (length gptel-ui--turns) 2))
      (should (cl-some (lambda (ov)
                         (and (overlay-get ov 'gptel-ui-code-opening)
                              (overlay-get ov 'before-string)))
                       gptel-ui--overlays)))))

(ert-deftest gptel-ui-org-quote-is-not-a-code-panel ()
  (gptel-ui-test--buffer
    (insert "#+begin_quote\nA quotation\n#+end_quote\n")
    (gptel-ui-mode 1)
    (should-not (cl-some (lambda (ov) (eq (overlay-get ov 'face) 'gptel-ui-code))
                        gptel-ui--overlays))))

(ert-deftest gptel-ui-source-and-disable-restore-local-settings ()
  (gptel-ui-test--buffer
    (setq-local line-spacing 0.1 header-line-format "Existing header")
    (let ((wrap-local (local-variable-p 'word-wrap)) (wrap word-wrap))
      (insert "@user\nHi\n")
      (gptel-ui-mode 1)
      (gptel-ui-toggle-source)
      (should-not gptel-ui--overlays)
      (gptel-ui-toggle-source)
      (should gptel-ui--overlays)
      (gptel-ui-mode -1)
      (should (equal line-spacing 0.1))
      (should (equal header-line-format "Existing header"))
      (should (eq (local-variable-p 'word-wrap) wrap-local))
      (should (eq word-wrap wrap))
      (should-not gptel-ui--timer))))

(ert-deftest gptel-ui-refresh-follows-streamed-and-restored-roles ()
  (gptel-ui-test--buffer
    (let ((gptel-response-prefix-alist nil))
      (insert "@user\nExplain\n\n")
      (gptel-ui-mode 1)
      (let ((beg (point)))
        (insert "# Answer\nStreaming content")
        (put-text-property beg (point) 'gptel 'response)
        (gptel-ui-refresh)
        (should (= (length gptel-ui--turns) 2))
        (should-not gptel-ui--timer)))))

(ert-deftest gptel-ui-preserves-native-header-status-slot ()
  (gptel-ui-test--buffer
    (let* ((gptel-mode t) (gptel-use-header-line t)
           (native (list "Backend" "Ready" '(:eval "Controls"))))
      (setq-local header-line-format native)
      (gptel-ui-mode 1)
      (setf (nth 1 header-line-format) "Streaming")
      (should-not (nth 2 header-line-format))
      (gptel-ui-mode -1)
      (should (equal header-line-format '("Backend" "Streaming" (:eval "Controls")))))))

(defun gptel-ui-test--insert-tool ()
  "Insert a representative native Org tool result."
  (insert "#+begin_tool (Read \"README.md\")")
  (insert (propertize "\n(:name \"Read\" :args (\"README.md\"))\n\nFile contents\n"
                      'gptel '(tool . "read-1")))
  (insert "#+end_tool\n\n"))

(ert-deftest gptel-ui-tool-cycle-is-one-assistant-turn ()
  (gptel-ui-test--buffer
    (insert "@user\nInspect this project.\n\n@mayuri\n")
    (dotimes (_ 4) (gptel-ui-test--insert-tool))
    (let ((final (point)))
      (insert (propertize "The final answer.\n" 'gptel 'response))
      (insert "\n@user\nFollow-up\n\n@mayuri\n")
      (insert (propertize "Another answer.\n" 'gptel 'response))
      (gptel-ui-mode 1)
      (should (= (length gptel-ui--turns) 4))
      (should-not (memq final gptel-ui--turns))
      (should (cl-some (lambda (ov) (and (overlay-get ov 'gptel-ui-tool-gap)
                                         (equal (overlay-get ov 'before-string) "\n\n")))
                       (overlays-at final)))
      (should (= (length gptel-ui--tool-response-starts) 1)))))

(ert-deftest gptel-ui-tool-first-without-prefix-has-one-label ()
  (gptel-ui-test--buffer
    (let ((gptel-response-prefix-alist nil))
      (insert "@user\nInspect this.\n\n")
      (let ((tool (point)))
        (gptel-ui-test--insert-tool)
        (insert (propertize "Final answer\n" 'gptel 'response))
        (gptel-ui-mode 1)
        (should (= (length gptel-ui--turns) 2))
        (should (memq tool gptel-ui--turns))
        (should (cl-some (lambda (ov) (and (overlay-get ov 'gptel-ui-block-opening)
                                           (overlay-get ov 'before-string)))
                        (overlays-at tool)))))))

(ert-deftest gptel-ui-repeated-prefix-is-hidden-without-changing-text ()
  (gptel-ui-test--buffer
    (insert "@user\nInspect this.\n\n@mayuri\n")
    (insert (propertize "Let me check.\n\n" 'gptel 'response))
    (gptel-ui-test--insert-tool)
    (let ((prefix (point)))
      (insert "@mayuri\n")
      (insert (propertize "The answer.\n" 'gptel 'response))
      (let ((text (buffer-string)))
        (gptel-ui-mode 1)
        (should (= (length gptel-ui--turns) 2))
        (should (cl-some (lambda (ov) (and (overlay-get ov 'gptel-ui-continuation)
                                           (equal (overlay-get ov 'display) "")))
                        (overlays-at prefix)))
        (should (equal-including-properties text (buffer-string)))))))

(ert-deftest gptel-ui-copy-response-does-not-paste-an-assistant-role ()
  (gptel-ui-test--buffer
    (let ((kill-ring nil) (interprogram-cut-function nil)
          (interprogram-paste-function nil))
      (insert "@mayuri\n")
      (let ((beg (point)))
        (insert (propertize "An answer" 'gptel 'response
                            'front-sticky '(gptel) 'face 'bold))
        (gptel-ui-mode 1)
        (let ((text (buffer-string)))
          (kill-ring-save beg (point-max))
          (should (eq (get-text-property 0 'face (car kill-ring)) 'bold))
          (should-not (get-text-property 0 'front-sticky (car kill-ring)))
          (should (equal-including-properties text (buffer-string))))
        (gptel-ui-test--buffer
          (insert "@user\nMy draft: ")
          (yank)
          (gptel-ui-mode 1)
          (should (equal (buffer-string) "@user\nMy draft: An answer"))
          (should-not (get-text-property (1- (point-max)) 'gptel))
          (should (= (length gptel-ui--turns) 1)))))))

(ert-deftest gptel-ui-copy-omits-response-prefixes-but-keeps-literal-examples ()
  (gptel-ui-test--buffer
    (insert "@mayuri\n")
    (insert (propertize "First answer.\n\n" 'gptel 'response))
    (insert "@mayuri\n")
    (insert (propertize "Second answer.\n\n```text\n@mayuri\n```\n"
                        'gptel 'response))
    (gptel-ui-mode 1)
    (let ((text (buffer-string)))
      (should (equal (filter-buffer-substring (point-max) (point-min))
                     "First answer.\n\nSecond answer.\n\n```text\n@mayuri\n```\n"))
      (should (equal-including-properties text (buffer-string))))))

(ert-deftest gptel-ui-copy-inline-and-partial-prefix-preserves-body-markup ()
  (gptel-ui-test--buffer
    (let ((gptel-response-prefix-alist '((text-mode . "### "))))
      (insert "### **Answer**")
      (gptel-ui-mode 1)
      (should (equal (filter-buffer-substring 2 (point-max)) "**Answer**"))
      (should (equal (filter-buffer-substring 5 9) "**An")))))

(ert-deftest gptel-ui-copy-preserves-source-and-cut-text ()
  (gptel-ui-test--buffer
    (insert "@mayuri\n")
    (insert (propertize "Answer" 'gptel 'response))
    (let ((text (buffer-string)))
      (gptel-ui-mode 1)
      (gptel-ui-toggle-source)
      (should (equal-including-properties
               text (filter-buffer-substring (point-min) (point-max))))
      (gptel-ui-toggle-source)
      (should (equal-including-properties
               text (filter-buffer-substring (point-min) (point-max) t)))
      (should (= (buffer-size) 0)))))

(ert-deftest gptel-ui-copy-composes-with-an-existing-buffer-filter ()
  (gptel-ui-test--buffer
    (let ((filter (lambda (beg end delete)
                    (upcase (buffer-substring--filter beg end delete)))))
      (setq-local filter-buffer-substring-function filter)
      (insert "@mayuri\nAnswer")
      (gptel-ui-mode 1)
      (should (equal (filter-buffer-substring (point-min) (point-max)) "ANSWER"))
      (gptel-ui-mode -1)
      (should (eq filter-buffer-substring-function filter))
      (should (equal (filter-buffer-substring (point-min) (point-max))
                     "@MAYURI\nANSWER")))))

(ert-deftest gptel-ui-copy-filter-is-local-and-restored ()
  (gptel-ui-test--buffer
    (let ((local (local-variable-p 'filter-buffer-substring-function))
          (filter filter-buffer-substring-function))
      (insert "@mayuri\nAnswer")
      (gptel-ui-mode 1)
      (with-temp-buffer
        (insert "@mayuri\nUnrelated")
        (should (equal (filter-buffer-substring (point-min) (point-max))
                       "@mayuri\nUnrelated")))
      (gptel-ui-mode -1)
      (should (eq (local-variable-p 'filter-buffer-substring-function) local))
      (should (eq filter-buffer-substring-function filter)))))

(ert-deftest gptel-ui-streaming-continuation-does-not-add-speaker ()
  (gptel-ui-test--buffer
    (insert "@user\nInspect this.\n\n@mayuri\n")
    (gptel-ui-test--insert-tool)
    (gptel-ui-mode 1)
    (should (= (length gptel-ui--turns) 2))
    (insert (propertize "A final response is streaming" 'gptel 'response))
    (gptel-ui-refresh)
    (should (= (length gptel-ui--turns) 2))))

(ert-deftest gptel-ui-empty-prefix-still-detects-next-user ()
  (gptel-ui-test--buffer
    (let ((gptel-prompt-prefix-alist nil) (gptel-response-prefix-alist nil))
      (insert "First user.\n\n")
      (insert (propertize "First answer.\n\n" 'gptel 'response))
      (gptel-ui-test--insert-tool)
      (insert (propertize "Continued answer.\n\n" 'gptel 'response))
      (insert "Second user.\n\n")
      (insert (propertize "Second answer.\n" 'gptel 'response))
      (gptel-ui-mode 1)
      (should (= (length gptel-ui--turns) 4)))))

(ert-deftest gptel-ui-org-quote-has-a-rail-and-preserves-native-fold ()
  (gptel-ui-test--buffer
    (insert "#+begin_quote\nQuoted text.\n#+end_quote\n")
    (let ((fold (make-overlay 14 27)))
      (add-to-invisibility-spec 'org-modern)
      (add-to-invisibility-spec 'org-hide-block)
      (put-text-property 1 14 'invisible 'org-modern)
      (overlay-put fold 'invisible 'org-hide-block)
      (let ((text (buffer-string)))
        (gptel-ui-mode 1)
        (should-not (invisible-p 1))
        (should (invisible-p 15))
        (should (eq (overlay-get fold 'invisible) 'org-hide-block))
        (should (cl-some (lambda (ov)
                           (and (overlay-get ov 'gptel-ui-quotation)
                                (string-match-p "│" (overlay-get ov 'line-prefix))))
                         gptel-ui--overlays))
        (should (equal-including-properties text (buffer-string)))))))

(provide 'gptel-ui-test)
;;; gptel-ui-test.el ends here
