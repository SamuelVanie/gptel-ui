;;; gptel-ui.el --- A quiet, readable gptel workspace -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Keywords: convenience
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Optional presentation for ordinary gptel chats, in Org or Markdown.
;; Enable `gptel-ui-global-mode' to dress existing and future chats.
;; All decorations are overlays: transcripts, role properties, and undo remain
;; untouched.  C-c C-v toggles source, C-c C-n / C-c C-p move between turns.

;;; Code:

(require 'gptel-ui-compat)

(require 'cl-lib)
(require 'subr-x)
(require 'face-remap)
(require 'gptel-ui-links)
(require 'gptel-ui-tables)
(require 'gptel-ui-tools)

(defgroup gptel-ui nil
  "Readable conversations without changing their contents."
  :group 'convenience :prefix "gptel-ui-")

(defcustom gptel-ui-width 100
  "Preferred reading width, in columns, including the side padding."
  :type 'integer :group 'gptel-ui)

(defcustom gptel-ui-refresh-delay 0.18
  "Seconds between display updates while a response is streaming."
  :type 'number :group 'gptel-ui)

(defcustom gptel-ui-assistant-name "Assistant"
  "Fallback name for responses without a named response prefix."
  :type 'string :group 'gptel-ui)

(defcustom gptel-ui-header-details nil
  "Whether to retain gptel's backend, model and prompt header controls.
The compact header always retains the native request status and highlights
pending tool approvals across conversations.  Re-enable `gptel-ui-mode'
after changing this option."
  :type 'boolean :group 'gptel-ui)

(defface gptel-ui-user
  '((t (:inherit font-lock-keyword-face :weight bold :height 0.95)))
  "Speaker label for the user's messages." :group 'gptel-ui)
(defface gptel-ui-assistant
  '((t (:inherit font-lock-function-name-face :weight bold :height 0.95)))
  "Speaker label for assistant responses." :group 'gptel-ui)
(defface gptel-ui-heading
  '((t (:inherit default :weight bold :height 1.18)))
  "Section heading." :group 'gptel-ui)
(defface gptel-ui-subheading
  '((t (:inherit default :weight semi-bold :height 1.06)))
  "Smaller section heading." :group 'gptel-ui)
(defface gptel-ui-muted
  '((t (:inherit shadow :height 0.9)))
  "Secondary information and structural punctuation." :group 'gptel-ui)
(defface gptel-ui-code
  '((((class color) (background dark))
     (:inherit fixed-pitch :background "#12333b" :extend t :weight normal :slant normal))
    (((class color) (background light))
     (:inherit fixed-pitch :background "#edf2f3" :extend t :weight normal :slant normal))
    (t (:inherit fixed-pitch)))
  "Code panel, keeping syntax highlighting from the major mode."
  :group 'gptel-ui)
(defface gptel-ui-code-label
  '((t (:inherit (gptel-ui-code shadow) :height 0.9 :weight bold)))
  "Language caption on a code panel." :group 'gptel-ui)
(defface gptel-ui-inline-code
  '((t (:inherit (fixed-pitch font-lock-constant-face))))
  "Inline code." :group 'gptel-ui)
(defface gptel-ui-report
  '((t (:inherit font-lock-type-face :weight bold)))
  "Discussion report caption." :group 'gptel-ui)
(defface gptel-ui-quote
  '((t (:inherit font-lock-doc-face :slant italic)))
  "Quoted prose." :group 'gptel-ui)
(defface gptel-ui-approval
  '((t (:inherit warning :weight bold)))
  "Pending tool approvals in the conversation header." :group 'gptel-ui)

(defvar gptel-mode)
(defvar gptel-model)
(defvar gptel-use-header-line)
(defvar gptel-prompt-prefix-alist)
(defvar gptel-response-prefix-alist)
(defvar gptel-ui-mode)
(declare-function gptel-highlight-mode "gptel" (&optional arg))

(defvar-local gptel-ui--overlays nil)
(defvar-local gptel-ui--timer nil)
(defvar-local gptel-ui--saved nil)
(defvar-local gptel-ui--face-cookie nil)
(defvar-local gptel-ui--window-margins nil)
(defvar-local gptel-ui--source nil)
(defvar-local gptel-ui--expanded-reports nil)
(defvar-local gptel-ui--excluded nil)
(defvar-local gptel-ui--indexed-ranges nil)
(defvar-local gptel-ui--range-index [])
(defvar-local gptel-ui--turns nil)
(defvar-local gptel-ui--tool-starts nil)
(defvar-local gptel-ui--tool-response-starts nil)
(defvar-local gptel-ui--highlight-was-on nil)
(defvar-local gptel-ui--native-header nil)
(defvar-local gptel-ui--installed-header nil)
(defvar-local gptel-ui--clone-state nil)
(defvar gptel-ui--approval-cache nil)
(defvar gptel-ui--approval-cache-dirty t)
(defvar gptel-ui--watching-approvals nil)

(defconst gptel-ui--approval-functions
  '(gptel--display-tool-calls gptel--accept-tool-calls
    gptel--reject-tool-calls gptel--steer-tool-calls)
  "Native operations that change pending approval prompts.")

(defun gptel-ui--overlay (beg end &rest properties)
  "Create an owned overlay from BEG to END with PROPERTIES."
  (let ((ov (make-overlay beg end nil nil nil)))
    (overlay-put ov 'gptel-ui t)
    (overlay-put ov 'evaporate t)
    (while properties
      (overlay-put ov (pop properties) (pop properties)))
    (push ov gptel-ui--overlays)
    ov))

(defun gptel-ui--clear ()
  "Remove only overlays owned by this mode."
  (mapc #'delete-overlay gptel-ui--overlays)
  (setq gptel-ui--overlays nil
        gptel-ui--turns nil
        gptel-ui--tool-starts nil
        gptel-ui--tool-response-starts nil
        gptel-ui--excluded nil
        gptel-ui--indexed-ranges nil
        gptel-ui--range-index []))

(defun gptel-ui--excluded-p (beg end)
  "Find an excluded range overlapping BEG through END in logarithmic time."
  (unless (eq gptel-ui--indexed-ranges gptel-ui--excluded)
    (setq gptel-ui--indexed-ranges gptel-ui--excluded
          gptel-ui--range-index
          (vconcat (sort (copy-sequence gptel-ui--excluded)
                         (lambda (a b) (< (car a) (car b)))))))
  (let ((lo 0) (hi (length gptel-ui--range-index)))
    (while (< lo hi)
      (let ((mid (/ (+ lo hi) 2)))
        (if (<= (cdr (aref gptel-ui--range-index mid)) beg)
            (setq lo (1+ mid))
          (setq hi mid))))
    (and (< lo (length gptel-ui--range-index))
         (< (car (aref gptel-ui--range-index lo)) end))))

(defun gptel-ui--skip-p (beg end)
  "Whether BEG through END overlaps code or native tool content."
  (or (let ((pos beg) found)
        (while (and (< pos end) (not found))
          (let ((role (get-text-property pos 'gptel)))
            (setq found (and role (not (eq role 'response)))))
          (setq pos (next-single-property-change pos 'gptel nil end)))
        found)
      (gptel-ui--excluded-p beg end)))

(defun gptel-ui--blocks ()
  "Give code, quotations and native tools distinct visual treatments."
  (goto-char (point-min))
  (let ((case-fold-search t))
    (while (re-search-forward
            "^[ \t]*\\(```+\\|~~~+\\|#\\+begin_\\([[:alnum:]_]+\\)\\)\\([^\n]*\\)$" nil t)
      (let* ((beg (line-beginning-position))
             (opening-end (line-end-position))
             (fence (match-string-no-properties 1))
             (kind (match-string-no-properties 2))
             (info (string-trim (match-string-no-properties 3)))
             (tool (or (equal (downcase (or kind "")) "tool")
                       (string-match-p "\\`tool\\b" info)))
             (closing (if kind
                          (concat "^[ \t]*#\\+end_" (regexp-quote kind) "[ \t]*$")
                        (format "^[ \t]*%s\\{%d,\\}[ \t]*$"
                                (regexp-quote (substring fence 0 1)) (length fence))))
             (body-beg (min (point-max) (1+ opening-end)))
             close-beg end)
        (forward-line 1)
        (if (re-search-forward closing nil t)
            (setq close-beg (line-beginning-position) end (line-end-position))
          (setq end (point-max)))
        (push (cons beg end) gptel-ui--excluded)
        (when tool
          (let ((decorations (gptel-ui-tools-render-block
                              beg opening-end body-beg close-beg end info)))
            (when decorations
              (push beg gptel-ui--tool-starts)
              (setq gptel-ui--overlays
                    (nconc decorations gptel-ui--overlays)))))
        (when (equal (downcase (or kind "")) "quote")
          (gptel-ui--overlay
           beg opening-end 'display (propertize "❝" 'face 'gptel-ui-quote)
           'face '(:inherit default :underline nil :overline nil :box nil)
           'invisible (unless (eq buffer-invisibility-spec t) 'gptel-ui-visible)
           'priority 20 'gptel-ui-block-opening t)
          (when close-beg
            (gptel-ui--overlay
             close-beg end 'display (propertize "❞" 'face 'gptel-ui-muted)
             'face '(:inherit default :underline nil :overline nil :box nil)
             'priority 20))
          (when (< body-beg (or close-beg end))
            (let ((rail (propertize "  │  " 'face 'gptel-ui-muted)))
              (gptel-ui--overlay
               body-beg (or close-beg end) 'face 'gptel-ui-quote
               'line-prefix rail 'wrap-prefix rail 'priority 2
               'gptel-ui-quotation t))))
        (when (and (not tool) (or (null kind) (member (downcase kind) '("src" "example"))))
          (gptel-ui--overlay beg end 'face 'gptel-ui-code 'priority 1)
          (gptel-ui--overlay
           beg opening-end 'face
           '(:inherit gptel-ui-code-label :underline nil :overline nil :box nil)
           'display
           (propertize (concat "  ┌  " (if (string-empty-p info) (or kind "code") info))
                       'face 'gptel-ui-code-label) 'priority 20
           'gptel-ui-code-opening t)
          (when close-beg
            (gptel-ui--overlay
             close-beg end 'face
             '(:inherit gptel-ui-code-label :underline nil :overline nil :box nil)
             'display (propertize "  └────────────────────"
				  'face 'gptel-ui-code-label)
             'priority 20))
          (when (< body-beg (or close-beg end))
            (gptel-ui--overlay
             body-beg (or close-beg end) 'line-prefix "  " 'wrap-prefix "  ")))
        (goto-char end)))))

(defun gptel-ui--speaker (beg end label assistant &optional prefix)
  "Label a turn at BEG through END with LABEL and ASSISTANT role.
PREFIX means an existing prefix is replaced, otherwise insert a caption."
  (let* ((face (if assistant 'gptel-ui-assistant 'gptel-ui-user))
         (caption (propertize (concat (if assistant "●  " "○  ") (upcase label))
                              'face face)))
    (push beg gptel-ui--turns)
    (if prefix
        (gptel-ui--overlay beg end 'display caption 'priority 30
                           'gptel-ui-copy-prefix assistant
                           'gptel-ui-copy-newline (eq (char-after end) ?\n))
      (if-let* ((opening (cl-find-if
                          (lambda (ov) (or (overlay-get ov 'gptel-ui-code-opening)
                                           (overlay-get ov 'gptel-ui-block-opening)))
                          (overlays-at beg))))
          (overlay-put opening 'before-string (concat caption "\n\n"))
        (gptel-ui--overlay
         beg (min (point-max) (1+ beg))
         'before-string (concat caption "\n\n") 'priority 30)))))

(defun gptel-ui--prefix (response)
  "Return this major mode's configured RESPONSE or prompt prefix."
  (let ((var (if response 'gptel-response-prefix-alist 'gptel-prompt-prefix-alist)))
    (and (boundp var) (alist-get major-mode (symbol-value var)))))

(defun gptel-ui--roles ()
  "Label each assistant turn once, including its tools and final response."
  (let* ((prompt (gptel-ui--prefix nil))
         (response (gptel-ui--prefix t))
         (name (if (and response (string-match "@[[:alnum:]_-]+" response))
                   (substring (match-string 0 response) 1)
                 gptel-ui-assistant-name))
         (pos (point-min)) events assistant-active tool-pending)
    ;; Gather boundaries first, then read them in transcript order.  A tool
    ;; result interrupts the response text property, not the speaker's turn.
    (dolist (spec (list (cons prompt nil) (cons response t)))
      (when (and (stringp (car spec)) (not (string-empty-p (car spec))))
        (goto-char (point-min))
        (while (re-search-forward
                (concat "^" (regexp-quote (string-remove-suffix "\n" (car spec)))
                        (when (string-suffix-p "\n" (car spec)) "$")) nil t)
          (let ((beg (match-beginning 0)) (end (match-end 0)))
            (unless (or (gptel-ui--skip-p beg end)
                        (and (not (cdr spec)) (eq (get-text-property beg 'gptel) 'response)))
              (push (list beg end (if (cdr spec) 'assistant-prefix 'user-prefix)
                          (string-suffix-p "\n" (car spec))) events))))))
    (dolist (beg gptel-ui--tool-starts)
      (push (list beg beg 'tool) events))
    (while (< pos (point-max))
      (let ((next (next-single-property-change pos 'gptel nil (point-max))))
        (when (eq (get-text-property pos 'gptel) 'response)
          (save-excursion
            (goto-char pos)
            (skip-chars-forward " \t\n" next)
            (let ((beg (point)))
              (when (< beg next)
                (push (list beg beg 'assistant) events)))))
        ;; Empty prompt prefixes are supported by gptel too.  In that case
        ;; ordinary untagged prose is the only available user boundary.
        (when (and (or (null prompt) (string-empty-p prompt))
                   (null (get-text-property pos 'gptel)))
          (save-excursion
            (goto-char pos)
            (skip-chars-forward " \t\n" next)
            (when (and (< (point) next)
                       (not (cl-some (lambda (event)
                                       (and (eq (nth 2 event) 'assistant-prefix)
                                            (= (car event) (point)))) events))
                       (not (gptel-ui--excluded-p (point) (1+ (point))))
                       (not (cl-some (lambda (ov) (overlay-get ov 'gptel-tool))
                                     (overlays-at (point)))))
              (push (list (point) (point) 'user) events))))
        (setq pos next)))
    (setq events
          (sort events (lambda (a b)
                         (if (= (car a) (car b))
                             (and (memq (nth 2 a) '(user-prefix assistant-prefix))
                                  (not (memq (nth 2 b) '(user-prefix assistant-prefix))))
                           (< (car a) (car b))))))
    (dolist (event events)
      (pcase-let ((`(,beg ,end ,kind . ,rest) event))
        (cond
         ((memq kind '(user-prefix user))
          (setq assistant-active nil tool-pending nil)
          (gptel-ui--speaker beg end "You" nil (eq kind 'user-prefix))
          (when (and (eq kind 'user-prefix) (not (car rest)))
            (overlay-put (car gptel-ui--overlays) 'after-string "\n\n")))
         (assistant-active
          ;; A literal repeated prefix is presentation noise as well.  Hide
          ;; only that prefix; keep every byte and role property in the buffer.
          (when (eq kind 'assistant-prefix)
            (gptel-ui--overlay
             beg (if (and (car rest) (< end (point-max))) (1+ end) end)
             'display "" 'priority 30 'gptel-ui-continuation t
             'gptel-ui-copy-prefix t)))
         (t
          (setq assistant-active t)
          (gptel-ui--speaker beg end name t (eq kind 'assistant-prefix))
          (when (and (eq kind 'assistant-prefix) (not (car rest)))
            (overlay-put (car gptel-ui--overlays) 'after-string "\n\n"))))
        (when (and (eq kind 'assistant) tool-pending)
          (push beg gptel-ui--tool-response-starts)
          (setq tool-pending nil))
        (when (eq kind 'tool) (setq tool-pending t))))))

(defun gptel-ui--filter-copy (function beg end delete)
  "Copy BEG through END using FUNCTION without assistant markers.
Keep native filtering, source view, and DELETE operations intact.  Only
rendered response prefixes and copied role metadata are omitted; the
transcript and ordinary markup remain untouched."
  (if (or (not gptel-ui-mode) gptel-ui--source delete)
      (funcall function beg end delete)
    (let ((start (min beg end)) (limit (max beg end)) ranges pieces)
      (dolist (ov gptel-ui--overlays)
        (when (and (overlay-buffer ov) (overlay-get ov 'gptel-ui-copy-prefix))
          (let ((from (max start (overlay-start ov)))
                (to (min limit (+ (overlay-end ov)
                                  (if (overlay-get ov 'gptel-ui-copy-newline) 1 0)))))
            (when (< from to) (push (cons from to) ranges)))))
      (dolist (range (sort ranges (lambda (a b) (< (car a) (car b)))))
        (when (< start (car range))
          (push (funcall function start (car range) nil) pieces))
        (setq start (max start (cdr range))))
      (when (< start limit)
        (push (funcall function start limit nil) pieces))
      (let ((text (apply #'concat (nreverse pieces))) (pos 0))
        ;; A pasted response property makes an ordinary draft look like a
        ;; new assistant response, even when no literal prefix was copied.
        (remove-list-of-text-properties 0 (length text) '(gptel) text)
        (while (< pos (length text))
          (let ((next (next-single-property-change pos 'front-sticky text (length text)))
                (sticky (get-text-property pos 'front-sticky text)))
            (when (and (listp sticky) (memq 'gptel sticky))
              (put-text-property pos next 'front-sticky (remq 'gptel sticky) text))
            (setq pos next)))
        text))))

(defun gptel-ui--tool-response-spacing ()
  "Add two display-only blank lines before prose following tool activity."
  (dolist (beg gptel-ui--tool-response-starts)
    ;; Attach to an existing replacement when the answer starts with a
    ;; heading, table, link or code block, so the gap stays visible too.
    (let ((ov (or (cl-find-if
                   (lambda (item) (and (= (overlay-start item) beg)
                                       (overlay-get item 'display)))
                   gptel-ui--overlays)
                  (gptel-ui--overlay beg (min (point-max) (1+ beg))
                                              'priority 30))))
      (overlay-put ov 'before-string (concat "\n\n" (overlay-get ov 'before-string)))
      (overlay-put ov 'gptel-ui-tool-gap t))))

(defun gptel-ui--headings ()
  "Style headings, list bullets, quotations, and inline emphasis."
  (goto-char (point-min))
  (while (re-search-forward
          (if (derived-mode-p 'org-mode) "^\\(\\*+\\) +\\(.+\\)$" "^\\(#+\\) +\\(.+\\)$") nil t)
    (let ((beg (match-beginning 0)) (end (match-end 0))
          (body (match-beginning 2)) (depth (length (match-string 1))))
      (unless (or (gptel-ui--skip-p beg end) (memq beg gptel-ui--turns))
        (gptel-ui--overlay beg body 'display "" 'priority 10)
        (gptel-ui--overlay body end 'face
                                    (if (<= depth 2) 'gptel-ui-heading
                                      'gptel-ui-subheading) 'priority 10))))
  (goto-char (point-min))
  (while (re-search-forward "^[ \t]*\\([-+]\\) \\|^\\(>\\) ?\\(.*\\)$" nil t)
    (let ((beg (match-beginning 0)) (end (match-end 0))
          (bullet (match-beginning 1)) (quote (match-beginning 2)))
      (unless (gptel-ui--skip-p beg end)
        (if bullet
            (gptel-ui--overlay bullet (1+ bullet) 'display "•" 'face 'gptel-ui-muted)
          (gptel-ui--overlay quote (1+ quote) 'display "│" 'face 'gptel-ui-muted)
          (gptel-ui--overlay (1+ quote) end 'face 'gptel-ui-quote)))))
  (dolist (spec '(("`\\([^`\n]+\\)`" . gptel-ui-inline-code)
                  ("\\*\\*\\([^*\n]+\\)\\*\\*" . bold)))
    (goto-char (point-min))
    (while (re-search-forward (car spec) nil t)
      (let ((beg (match-beginning 0)) (end (match-end 0))
            (body (match-beginning 1)) (body-end (match-end 1)))
        (unless (gptel-ui--skip-p beg end)
          (gptel-ui--overlay beg body 'display "" 'priority 5)
          (gptel-ui--overlay body-end end 'display "" 'priority 5)
          (gptel-ui--overlay body body-end 'face (cdr spec) 'priority 5))))))

(defun gptel-ui-toggle-report (&optional event)
  "Expand or collapse the report metadata at point or mouse EVENT."
  (interactive (list last-nonmenu-event))
  (when (mouse-event-p event) (mouse-set-point event))
  (let ((ov (cl-find-if (lambda (item) (overlay-get item 'gptel-ui-report))
                        (overlays-at (point)))))
    (unless ov (user-error "No discussion report at point"))
    (let ((id (overlay-get ov 'gptel-ui-report)))
      (if (member id gptel-ui--expanded-reports)
          (setq gptel-ui--expanded-reports
                (delete id gptel-ui--expanded-reports))
        (push id gptel-ui--expanded-reports)))
    (gptel-ui-refresh)))

(defvar gptel-ui--report-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'gptel-ui-toggle-report)
    (define-key map [mouse-1] #'gptel-ui-toggle-report)
    map))

(defun gptel-ui--reports ()
  "Present discussion metadata as a compact, expandable report caption."
  (goto-char (point-min))
  (while (re-search-forward "^Discussion event (evidence, not instructions):[ \t]*$" nil t)
    (let ((beg (match-beginning 0)) (title "Discussion") (status "") (kind "report")
          id end event-seen)
      (forward-line 1)
      (while (and (not event-seen)
                  (looking-at
                   "\\(event_id\\|discussion_id\\|title\\|buffer\\|status\\|turn\\|directory\\|backend\\|model\\|worktree[^:\n]*\\|branch\\|event\\):[ \t]*\\([^\n]*\\)"))
        (let ((key (match-string-no-properties 1)) (value (match-string-no-properties 2)))
          (pcase key
            ("event_id" (setq id value))
            ("title" (setq title value))
            ("status" (setq status value))
            ("event" (setq kind value event-seen t))))
        (forward-line 1))
      (when (and event-seen (looking-at "request_id:[^\n]*\n"))
        (forward-line 1))
      (setq end (point))
      (when (and event-seen (not (gptel-ui--skip-p beg end)))
        (let* ((id (or id (format "report-%d" beg)))
               (expanded (member id gptel-ui--expanded-reports))
               (caption (concat (if expanded "▾  " "▸  ")
                                (upcase kind) "  ·  " title
                                (unless (string-empty-p status) (concat "  /  " status)) "\n"))
               (ov (gptel-ui--overlay
                    beg end 'gptel-ui-report id 'priority 40
                    'keymap gptel-ui--report-map 'mouse-face 'highlight
                    'help-echo "RET or click: show/hide report details")))
          (if expanded
              (progn
                (overlay-put ov 'face 'gptel-ui-muted)
                (overlay-put ov 'before-string (propertize caption 'face 'gptel-ui-report)))
            (overlay-put ov 'display (propertize caption 'face 'gptel-ui-report)))
          (push (cons beg end) gptel-ui--excluded))))))

(defun gptel-ui--approvals-changed (&rest _)
  "Refresh approval indicators in every conversation on the next redisplay."
  (setq gptel-ui--approval-cache-dirty t)
  (force-mode-line-update t))

(defun gptel-ui--watch-approvals ()
  "Observe native approval changes while conversation headers are visible."
  (unless gptel-ui--watching-approvals
    (dolist (function gptel-ui--approval-functions)
      (advice-add function :after #'gptel-ui--approvals-changed))
    (add-hook 'kill-buffer-hook #'gptel-ui--approvals-changed)
    (setq gptel-ui--watching-approvals t))
  (gptel-ui--approvals-changed))

(defun gptel-ui--unwatch-approvals ()
  "Stop observing approvals when the last conversation view closes."
  (dolist (function gptel-ui--approval-functions)
    (advice-remove function #'gptel-ui--approvals-changed))
  (remove-hook 'kill-buffer-hook #'gptel-ui--approvals-changed)
  (setq gptel-ui--watching-approvals nil
        gptel-ui--approval-cache nil
        gptel-ui--approval-cache-dirty t))

(defun gptel-ui--header-next-approval (event)
  "Visit the next pending approval from the header clicked by EVENT."
  (interactive "e")
  (let ((window (posn-window (event-start event))))
    (when (window-live-p window) (select-window window)))
  (gptel-ui-next-approval))

(defvar gptel-ui--approval-header-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] #'gptel-ui--header-next-approval)
    (define-key map [mouse-1] #'gptel-ui--header-next-approval)
    map)
  "Navigation from the pending-approval indicator.")

(defun gptel-ui--approval-header ()
  "Return a clickable indicator for pending approvals in any conversation."
  (when gptel-ui--approval-cache-dirty
    (setq gptel-ui--approval-cache (gptel-ui-tools-pending-approvals)
          gptel-ui--approval-cache-dirty nil))
  ;; Buffer deletion and cancellation can remove overlays outside native
  ;; approval commands.  Never keep a dead prompt visible in the header.
  (setq gptel-ui--approval-cache
        (cl-remove-if-not #'overlay-buffer gptel-ui--approval-cache))
  (when gptel-ui--approval-cache
    (let* ((count (length gptel-ui--approval-cache))
           (buffers (delete-dups
                     (mapcar (lambda (ov) (buffer-name (overlay-buffer ov)))
                             gptel-ui--approval-cache))))
      (propertize
       (format "  ! %d approval%s  " count (if (= count 1) "" "s"))
       'face 'gptel-ui-approval 'mouse-face 'highlight
       'local-map gptel-ui--approval-header-map
       'help-echo
       (concat "Pending tool approvals: " (string-join buffers ", ")
               "\nClick to visit the next approval, or "
               (substitute-command-keys "\\[gptel-ui-next-approval]"))))))

(defun gptel-ui--header ()
  "Return a compact conversation header, with approvals before other details."
  (concat (gptel-ui--approval-header)
          (propertize "  ◉  CHAT" 'face 'gptel-ui-assistant)
          (propertize (concat "    " (file-name-nondirectory (directory-file-name default-directory)))
                      'face 'gptel-ui-muted)
          (propertize (cond (gptel-ui--source "   SOURCE")
                            ((bound-and-true-p gptel-use-header-line) "")
                            (t "     C-c C-v  source")) 'face 'gptel-ui-muted)
          "  "))

(defun gptel-ui--setup-header ()
  "Install a compact header while preserving native status updates."
  (setq gptel-ui--native-header
        (and (bound-and-true-p gptel-mode)
             (bound-and-true-p gptel-use-header-line)
             (consp header-line-format)
             header-line-format))
  (setq-local header-line-format
              (if gptel-ui--native-header
                  ;; Native gptel updates slot 1 in place.  The remaining
                  ;; controls stay in the saved header for exact restoration.
                  (append (list (if gptel-ui-header-details
                                    (list '(:eval (gptel-ui--header))
                                          (car gptel-ui--native-header))
                                  '(:eval (gptel-ui--header)))
                                (nth 1 gptel-ui--native-header))
                          (when gptel-ui-header-details
                            (nthcdr 2 gptel-ui--native-header)))
                (list '(:eval (gptel-ui--header)) header-line-format)))
  (setq gptel-ui--installed-header header-line-format)
  (gptel-ui--watch-approvals))

(defun gptel-ui--restore-header-status ()
  "Keep the latest native request status when restoring the original header."
  (when (and gptel-ui--native-header
             (eq header-line-format gptel-ui--installed-header))
    (setf (nth 1 gptel-ui--native-header) (nth 1 header-line-format)))
  (setq gptel-ui--native-header nil gptel-ui--installed-header nil))

(defun gptel-ui--margins ()
  "Give every window displaying the current buffer a comfortable measure."
  (dolist (window (get-buffer-window-list (current-buffer) nil t))
    (unless (assq window gptel-ui--window-margins)
      (push (cons window (window-margins window)) gptel-ui--window-margins))
    (let ((margin (max 2 (/ (- (window-total-width window) gptel-ui-width) 2))))
      (unless (equal (window-margins window) (cons margin margin))
        (set-window-margins window margin margin)))))

;;;###autoload
(defun gptel-ui-refresh (&rest _)
  "Refresh the current chat's display without modifying its transcript."
  (interactive)
  (when gptel-ui-mode
    (when (timerp gptel-ui--timer) (cancel-timer gptel-ui--timer))
    (setq gptel-ui--timer nil)
    (save-excursion
      (save-restriction
        (widen)
        (gptel-ui--clear)
        (gptel-ui--margins)
        (unless gptel-ui--source
          (gptel-ui--blocks)
          (gptel-ui--reports)
          (gptel-ui--roles)
          (gptel-ui--headings)
          (let ((tables (gptel-ui-tables-render
                         (point-min) (point-max) #'gptel-ui--skip-p)))
            (dolist (ov tables)
              (push (cons (overlay-start ov) (overlay-end ov)) gptel-ui--excluded))
            (setq gptel-ui--overlays (nconc tables gptel-ui--overlays)))
          (setq gptel-ui--overlays
                (nconc (gptel-ui-links-render
                        (point-min) (point-max) #'gptel-ui--skip-p)
                       gptel-ui--overlays))
          (gptel-ui--tool-response-spacing)
          (setq gptel-ui--turns (sort (delete-dups gptel-ui--turns) #'<)))))))

(defun gptel-ui--schedule (&rest _)
  "Coalesce streaming and editing changes into one display refresh."
  (when (and gptel-ui-mode (not (timerp gptel-ui--timer)))
    (setq gptel-ui--timer
          (run-at-time gptel-ui-refresh-delay nil
                       (lambda (buffer)
                         (when (buffer-live-p buffer)
                           (with-current-buffer buffer (gptel-ui-refresh))))
                       (current-buffer)))))

(defun gptel-ui--window-change (&optional frame)
  "Schedule a responsive table layout in windows of FRAME."
  (dolist (window (window-list (and (framep frame) frame) 'no-minibuffer))
    (with-current-buffer (window-buffer window)
      (when gptel-ui-mode (gptel-ui--schedule)))))

(defun gptel-ui-toggle-source ()
  "Toggle the untouched source text for editing or inspection."
  (interactive)
  (setq gptel-ui--source (not gptel-ui--source))
  (gptel-ui-refresh)
  (force-mode-line-update))

(defun gptel-ui-next-turn (&optional previous)
  "Move to the next turn, or PREVIOUS turn when non-nil."
  (interactive)
  (let ((destination (if previous
                         (car (last (cl-remove-if-not (lambda (pos) (< pos (point)))
                                                      gptel-ui--turns)))
                       (cl-find-if (lambda (pos) (> pos (point))) gptel-ui--turns))))
    (if destination (goto-char destination) (user-error "No %s turn" (if previous "previous" "next")))))

(defun gptel-ui-previous-turn ()
  "Move to the previous conversation turn."
  (interactive)
  (gptel-ui-next-turn t))

(defvar gptel-ui-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-v") #'gptel-ui-toggle-source)
    (define-key map (kbd "C-c C-n") #'gptel-ui-next-turn)
    (define-key map (kbd "C-c C-p") #'gptel-ui-previous-turn)
    (define-key map (kbd "C-c ]") #'gptel-ui-next-approval)
    (define-key map (kbd "C-c [") #'gptel-ui-previous-approval)
    (define-key map (kbd "C-c C-o") #'gptel-ui-links-open)
    map))

(defun gptel-ui--cleanup ()
  "Cancel the pending renderer and remove decorations."
  (when (timerp gptel-ui--timer) (cancel-timer gptel-ui--timer))
  (setq gptel-ui--timer nil)
  (gptel-ui--clear)
  (unless (cl-some (lambda (buffer)
                     (and (not (eq buffer (current-buffer)))
                          (buffer-local-value 'gptel-ui-mode buffer)))
                   (buffer-list))
    (gptel-ui--unwatch-approvals)
    (remove-hook 'window-size-change-functions #'gptel-ui--window-change)
    (remove-hook 'window-configuration-change-hook #'gptel-ui--window-change)))

;;;###autoload
(define-minor-mode gptel-ui-mode
  "A spacious, navigable reading view for Org and Markdown chats.
All display changes are reversible.
Use `gptel-ui-toggle-source' to view or edit the raw source."
  :lighter " Atelier" :keymap gptel-ui-mode-map
  (if gptel-ui-mode
      (unless gptel-ui--saved
        (dolist (var '(line-spacing word-wrap truncate-lines header-line-format
                                    filter-buffer-substring-function
                                    display-line-numbers))
          (push (list var (local-variable-p var) (symbol-value var)) gptel-ui--saved))
        (setq-local line-spacing 0.22 word-wrap t truncate-lines nil display-line-numbers nil)
        (gptel-ui--setup-header)
        (setq gptel-ui--face-cookie
              (face-remap-add-relative 'default '(:weight normal :slant normal) 'variable-pitch))
        (setq gptel-ui--highlight-was-on (bound-and-true-p gptel-highlight-mode))
        (when gptel-ui--highlight-was-on (gptel-highlight-mode -1))
        (add-function :around (local 'filter-buffer-substring-function)
                      #'gptel-ui--filter-copy)
        (add-hook 'after-change-functions #'gptel-ui--schedule nil t)
        (add-hook 'gptel-post-stream-hook #'gptel-ui--schedule nil t)
        (add-hook 'gptel-post-response-functions #'gptel-ui--schedule nil t)
        (add-hook 'kill-buffer-hook #'gptel-ui--cleanup nil t)
        (add-hook 'change-major-mode-hook #'gptel-ui--disable nil t)
        (add-hook 'window-size-change-functions #'gptel-ui--window-change)
        (add-hook 'window-configuration-change-hook #'gptel-ui--window-change)
        (gptel-ui-refresh)
        ;; Restored chats receive role properties after gptel-mode-hook.
        (gptel-ui--schedule))
    (gptel-ui--restore-header-status)
    (gptel-ui--cleanup)
    (remove-function (local 'filter-buffer-substring-function) #'gptel-ui--filter-copy)
    (remove-hook 'after-change-functions #'gptel-ui--schedule t)
    (remove-hook 'gptel-post-stream-hook #'gptel-ui--schedule t)
    (remove-hook 'gptel-post-response-functions #'gptel-ui--schedule t)
    (remove-hook 'kill-buffer-hook #'gptel-ui--cleanup t)
    (remove-hook 'change-major-mode-hook #'gptel-ui--disable t)
    (when gptel-ui--face-cookie
      (face-remap-remove-relative gptel-ui--face-cookie)
      (setq gptel-ui--face-cookie nil))
    (dolist (saved gptel-ui--saved)
      (if (nth 1 saved) (set (make-local-variable (car saved)) (nth 2 saved))
        (kill-local-variable (car saved))))
    (dolist (entry gptel-ui--window-margins)
      (when (and (window-live-p (car entry)) (eq (window-buffer (car entry)) (current-buffer)))
        (set-window-margins (car entry) (cadr entry) (cddr entry))))
    (setq gptel-ui--saved nil gptel-ui--window-margins nil)
    (when (and gptel-ui--highlight-was-on (bound-and-true-p gptel-mode))
      (gptel-highlight-mode 1))
    (setq gptel-ui--highlight-was-on nil)))

(defun gptel-ui--disable ()
  "Disable the chat view before a major mode change."
  (gptel-ui-mode -1))

(defun gptel-ui--clone ()
  "Detach a cloned conversation from its source's presentation ownership."
  (when gptel-ui-mode
    (let ((saved gptel-ui--saved)
          (header (nth 2 (assq 'header-line-format gptel-ui--saved))))
      ;; Only the header's mutable spine needs copying.  Keep the identities
      ;; of native info segments and embedded function closures intact.
      (when (consp header) (setq header (copy-sequence header)))
      ;; The saved native header can lag behind the displayed status.  Update
      ;; only the clone's copy, never the source's saved header cons cells.
      (when (and gptel-ui--native-header
                 (eq header-line-format gptel-ui--installed-header))
        (setf (nth 1 header) (nth 1 header-line-format)))
      ;; These locals were copied by reference.  Do not disable the mode or
      ;; run its cleanup before dropping the source's overlays and timer.
      (setq gptel-ui-mode nil
            gptel-ui--clone-state (list gptel-ui--highlight-was-on)
            gptel-ui--overlays nil gptel-ui--timer nil
            gptel-ui--saved nil gptel-ui--window-margins nil
            gptel-ui--native-header nil gptel-ui--installed-header nil
            gptel-ui--excluded nil gptel-ui--indexed-ranges nil
            gptel-ui--range-index [] gptel-ui--turns nil
            gptel-ui--tool-starts nil gptel-ui--tool-response-starts nil
            gptel-ui--expanded-reports (copy-sequence gptel-ui--expanded-reports))
      ;; Optional renderer caches must not retain source-owned link overlays.
      (dolist (variable '(gptel-ui--link-cache gptel-ui--link-dirty-start))
        (when (local-variable-p variable) (set variable nil)))
      ;; Face-remap removal mutates the entry's list spine.  Retain its spec
      ;; identities for the cookie while making that spine private first.
      (setq-local face-remapping-alist
                  (mapcar (lambda (entry)
                            (cons (car entry)
                                  (if (consp (cdr entry)) (copy-sequence (cdr entry))
                                    (cdr entry))))
                          face-remapping-alist))
      (when gptel-ui--face-cookie
        (face-remap-remove-relative gptel-ui--face-cookie))
      (setq gptel-ui--face-cookie nil)
      ;; Restore the pre-view settings directly.  In particular, discard the
      ;; inherited copy-filter wrapper instead of modifying shared advice.
      (dolist (entry saved)
        (if (nth 1 entry)
            (set (make-local-variable (car entry))
                 (if (eq (car entry) 'header-line-format) header (nth 2 entry)))
          (kill-local-variable (car entry))))
      ;; add-hook/remove-hook may edit these lists while the clone is alive.
      (dolist (hook '(after-change-functions gptel-post-stream-hook
                      gptel-post-response-functions kill-buffer-hook
                      change-major-mode-hook))
        (when (local-variable-p hook)
          (set hook (copy-sequence (symbol-value hook))))))))

(defun gptel-ui--clone-enable ()
  "Rebuild the cloned view after other packages have detached native state."
  (when gptel-ui--clone-state
    (let ((highlight (car gptel-ui--clone-state)))
      (setq gptel-ui--clone-state nil)
      (gptel-ui-mode 1)
      (setq gptel-ui--highlight-was-on highlight))))

;; Run before other packages' clone hooks can refresh or close the new chat.
(add-hook 'clone-buffer-hook #'gptel-ui--clone -100)
(add-hook 'clone-indirect-buffer-hook #'gptel-ui--clone -100)
(add-hook 'clone-buffer-hook #'gptel-ui--clone-enable 100)
(add-hook 'clone-indirect-buffer-hook #'gptel-ui--clone-enable 100)

(defun gptel-ui--maybe-enable ()
  "Follow gptel's minor mode in conversation buffers."
  (if (bound-and-true-p gptel-mode) (gptel-ui-mode 1)
    (when gptel-ui-mode
      ;; gptel has already restored its pre-chat header before this hook.
      (let ((header header-line-format))
        (gptel-ui-mode -1)
        (setq header-line-format header)))))

;;;###autoload
(define-minor-mode gptel-ui-global-mode
  "Automatically dress existing and future gptel conversation buffers."
  :global t :group 'gptel-ui
  (if gptel-ui-global-mode
      (progn
        (add-hook 'gptel-mode-hook #'gptel-ui--maybe-enable t)
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer
            (when (bound-and-true-p gptel-mode) (gptel-ui-mode 1)))))
    (remove-hook 'gptel-mode-hook #'gptel-ui--maybe-enable)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when gptel-ui-mode (gptel-ui-mode -1))))))

;;;###autoload
(defun gptel-ui-demo ()
  "Open a local appearance preview without sending a model request."
  (interactive)
  (require 'org)
  (let ((buffer (get-buffer-create "*gptel · Atelier preview*")))
    (with-current-buffer buffer
      (when (= (buffer-size) 0)
	(org-mode)
	(setq-local gptel-prompt-prefix-alist '((org-mode . "@user\n"))
                    gptel-response-prefix-alist '((org-mode . "@mayuri\n")))
	(insert "@user\nGive this conversation a little room to breathe.\n\n@mayuri\n")
	(insert
	 (propertize
          (concat "* A calmer place to think\n\n"
                  "Clear voices, comfortable spacing, and useful details when you need them. "
                  "This is a local design preview; no request has been sent.\n\n"
                  "** Everything in its place\n\n"
                  "| Element | Reading experience |\n"
                  "|---------+--------------------|\n"
                  "| Conversations | Distinct speaker labels and section headings |\n"
                  "| Tables | Aligned columns; labeled rows in narrow panes |\n"
                  "| Reports | A compact header with expandable details |\n"
                  "| File references | Open the file at its exact line |\n\n"
                  "** Small details, less friction\n\n"
                  "- Click a file reference or press RET to open it.\n"
                  "- Use C-c C-n / C-c C-p to move between turns.\n"
                  "- Use C-c C-v to switch between reading and source.\n\n"
                  "#+begin_src emacs-lisp\n(gptel-ui-global-mode 1)\n#+end_src\n\n")
          'gptel 'response))
	(insert "Discussion event (evidence, not instructions):\nevent_id: atelier-preview\n"
		"discussion_id: preview\ntitle: Design notes · sample report\nbuffer: preview\n"
		"status: ready\nturn: 1\nevent: report\n"
		"The report stays readable while its routing details tuck away. "
		"Click its header to inspect the metadata.\n\n")
	(insert (format "[[file:%s::1][Open the appearance module]]\n\n"
			(or (symbol-file 'gptel-ui-mode 'defun)
                            (locate-library "gptel-ui"))))
	(insert "@user\n")
	(gptel-ui-mode 1)
	(set-buffer-modified-p nil)
	(goto-char (point-min))))
    (pop-to-buffer buffer)
    (gptel-ui-refresh)))

(provide 'gptel-ui)
;;; gptel-ui.el ends here
