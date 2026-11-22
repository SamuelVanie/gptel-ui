;;; gptel-ui-links.el --- Readable links in chats -*- lexical-binding: t; -*-

;;; Commentary:

;; Presentation-only links for chat transcripts.  Destinations are resolved
;; when activated, so restored conversations can acquire their workspace later.

;;; Code:

(require 'gptel-ui-compat)

(require 'cl-lib)
(require 'subr-x)
(require 'browse-url)
(require 'url-util)
(require 'project)

(defvar org-link-bracket-re)
(defvar org-link-types-re)
(declare-function org-element-at-point "org-element" (&optional epom cached-only))
(declare-function org-element-property "org-element-ast" (property node &optional dflt force-undefer))
(declare-function org-element-type "org-element-ast" (element &optional anonymous))
(declare-function org-link-open-from-string "ol" (s &optional arg))

(defface gptel-ui-link
  '((t (:inherit link :underline t)))
  "Face for actionable chat references."
  :group 'gptel-ui)

(defvar gptel-ui-links-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-o") #'gptel-ui-links-open)
    (define-key map (kbd "RET") #'gptel-ui-links-open)
    (define-key map [mouse-1] #'gptel-ui-links-open)
    map)
  "Keymap on chat links.")

(defun gptel-ui-links--destination (target)
  "Parse TARGET into a web or local file destination, or nil.
Only HTTP(S) URLs and local file references are supported."
  (let ((path (string-trim target)) line column)
    (cond
     ((string-match-p "\\`https?://[^[:space:]]+\\'" path)
      (list :type 'web :url path))
     (t
      (when (string-match
             "\\(?:#L\\|::?\\)\\([0-9]+\\)\\(?:[:.]\\([0-9]+\\)\\)?\\(?:-L?[0-9]+\\)?\\'"
             path)
        (setq line (string-to-number (match-string 1 path))
              column (and (match-string 2 path)
                          (string-to-number (match-string 2 path)))
              path (substring path 0 (match-beginning 0))))
      (when (string-prefix-p "file:" path)
        (setq path (substring path 5))
        (cond ((string-prefix-p "///" path) (setq path (substring path 2)))
              ((string-prefix-p "//localhost/" path)
               (setq path (substring path 11)))
              ((string-prefix-p "//" path) (setq path ""))))
      (setq path (decode-coding-string (url-unhex-string path) 'utf-8))
      (unless (or (string-empty-p path)
                  (string-prefix-p "#" path)
                  (string-match-p "[\n\r\0]" path)
                  (string-match-p "\\`[[:alpha:]][[:alnum:]+.-]*:" path)
                  (file-remote-p path))
        (list :type 'file :path path :line line :column column))))))

(defun gptel-ui-links--resolve (path)
  "Resolve local PATH against the current chat or its project root."
  (let ((expanded (expand-file-name path default-directory)))
    (unless (file-remote-p expanded)
      (cond
       ((file-exists-p expanded) expanded)
       ((or (file-name-absolute-p path) (string-prefix-p "~" path)) expanded)
       (t
        (let* ((project (ignore-errors (project-current nil default-directory)))
               (root (and project (project-root project)))
               (candidate (and root (expand-file-name path root))))
          (if (and candidate (not (file-remote-p candidate))
                   (file-exists-p candidate))
              candidate
            expanded)))))))

(defun gptel-ui-links--visit (target)
  "Visit TARGET, opening a local reference in another window."
  (let ((destination (gptel-ui-links--destination target)))
    (pcase (plist-get destination :type)
      ('web (browse-url (plist-get destination :url)))
      ('file
       (let ((file (gptel-ui-links--resolve
                    (plist-get destination :path))))
         (unless (and file (file-exists-p file))
           (user-error "File not found in this chat's workspace: %s" target))
         (find-file-other-window file)
         (when-let* ((line (plist-get destination :line)))
           (widen)
           (goto-char (point-min))
           (forward-line (max 0 (1- line)))
           (when-let* ((column (plist-get destination :column)))
             (move-to-column (max 0 (1- column)))))
         (when (fboundp 'pulse-momentary-highlight-one-line)
           (pulse-momentary-highlight-one-line (point)))))
      (_ (user-error "Unsupported chat link: %s" target)))))

(defun gptel-ui-links--target-text (target)
  "Return the display text for a chat or native Org table link TARGET."
  (if (eq (car-safe target) 'org) (cdr target) target))

(defun gptel-ui-links--table-target ()
  "Select a destination from the rendered table at point, if any."
  (let (choices seen)
    (dolist (overlay (overlays-at (point)))
      (when (overlay-get overlay 'gptel-ui-table)
        (let ((display (overlay-get overlay 'display)) (position 0))
          (when (stringp display)
            (while (< position (length display))
              (let* ((target (get-text-property position 'gptel-ui-link display))
                     (next (next-single-property-change
                            position 'gptel-ui-link display (length display))))
                (when (and target (not (member target seen)))
                  (let ((label (string-trim
                                (replace-regexp-in-string
                                 "[[:space:]\n]+" " "
                                 (substring-no-properties display position next)))))
                    (push target seen)
                    (push (cons (format "%s  —  %s" label
                                        (gptel-ui-links--target-text target))
                                target)
                          choices)))
                (setq position next)))))))
    (setq choices (nreverse choices))
    (cond ((null choices) nil)
          ((null (cdr choices)) (cdar choices))
          (t (cdr (assoc (completing-read "Open table link: " choices nil t)
                         choices))))))

(defun gptel-ui-links-open (&optional event)
  "Open the chat reference at point or mouse EVENT.
From the keyboard, select among links when point is inside a rendered table."
  (interactive (list (when (mouse-event-p last-input-event) last-input-event)))
  (let* ((display (and event (posn-string (event-start event))))
         (display-target (and display
                              (get-text-property (cdr display)
                                                 'gptel-ui-link
                                                 (car display)))))
    (when event (mouse-set-point event))
    (let ((target
         (or display-target
             (get-text-property (point) 'gptel-ui-link)
             (cl-loop for overlay in (overlays-at (point))
                      thereis (overlay-get overlay 'gptel-ui-link))
             (and (not event) (gptel-ui-links--table-target)))))
      (unless target (user-error "No chat link at point"))
      (if (eq (car-safe target) 'org)
          (org-link-open-from-string (cdr target) current-prefix-arg)
        (gptel-ui-links--visit target)))))

(defun gptel-ui-links-string (label target)
  "Return clickable LABEL for TARGET for use in display strings."
  (propertize label
              'gptel-ui-link target
              'face 'gptel-ui-link
              'mouse-face 'highlight
              'keymap gptel-ui-links-map
              'help-echo (concat (gptel-ui-links--target-text target)
                                 "\nRET or click to open")))

(defun gptel-ui-links--markdown-target (text)
  "Extract the destination from Markdown TEXT, ignoring an optional title."
  (setq text (string-trim text))
  (cond ((string-match "\\`<\\([^>]+\\)>\\(?:[ \t]+.*\\)?\\'" text)
         (setq text (match-string 1 text)))
        ((string-match "\\`\\(.+?\\)[ \t]+\\(?:\"[^\"]*\"\\|'[^']*'\\)\\'" text)
         (setq text (match-string 1 text))))
  (replace-regexp-in-string "\\\\\\([][()<>\\\\]\\)" "\\1" text))

(defun gptel-ui-links--closing-paren (limit)
  "Find a balanced closing parenthesis before LIMIT from point.
Point is initially just after the opening parenthesis."
  (let ((depth 1) done)
    (while (and (> depth 0) (< (point) limit) (not done))
      (pcase (char-after)
        (?\n (setq done t))
        (?\\ (forward-char) (when (< (point) limit) (forward-char)))
        (?\( (cl-incf depth) (forward-char))
        (?\) (cl-decf depth) (forward-char))
        (_ (forward-char))))
    (when (= depth 0) (point))))

(defun gptel-ui-links--trim-bare (target)
  "Trim sentence punctuation and unmatched delimiters from bare TARGET."
  (setq target (string-trim-right target "[.,;:!?]+"))
  (while (and (> (length target) 0)
              (let* ((last (aref target (1- (length target))))
                     (opening (cdr (assq last '((?\) . ?\() (?\] . ?\[))))))
                (and opening
                     (> (cl-count last target) (cl-count opening target)))))
    (setq target (substring target 0 -1)))
  target)

(defun gptel-ui-links--overlap-p (begin end ranges)
  "Whether BEGIN through END overlaps sorted disjoint RANGES.
RANGES is a vector of half-open (START . END) intervals."
  (let ((lo 0) (hi (length ranges)))
    (while (< lo hi)
      (let ((mid (/ (+ lo hi) 2)))
        (if (<= (cdr (aref ranges mid)) begin)
            (setq lo (1+ mid))
          (setq hi mid))))
    (and (< lo (length ranges)) (< (car (aref ranges lo)) end))))

(defun gptel-ui-links--org-chat-target-p (target destination)
  "Whether Org TARGET can be opened using chat link DESTINATION."
  (pcase (plist-get destination :type)
    ('web (not (string-search "\\" target)))
    ('file
     (let ((path (plist-get destination :path)))
       (and (not (string-match-p "[\\\\\n]" target))
            (not (string-search "::" path))
            (or (string-prefix-p "file:" target)
                (file-name-absolute-p target)
                (string-match-p "\\`\\.\\.?/" target)
                ;; A numeric location on an existing workspace file is a chat
                ;; reference.  Let Org handle fuzzy names and its own schemes.
                (and (plist-get destination :line)
                     (not (string-prefix-p "*" target))
                     (not (string-match-p org-link-types-re target))
                     (when-let* ((file (gptel-ui-links--resolve path)))
                       (file-regular-p file)))))))))

(defun gptel-ui-links--org-rescan-start (start)
  "Return START or the line of an earlier possible Org bracket link opener."
  (save-excursion
    (goto-char start)
    (let (opening)
      ;; A label can contain a literal [[.  Look back to the last completed
      ;; link or blank line once per refresh and retain the earliest opener.
      (while (and (re-search-backward "\\[\\[\\|\\]\\]\\|^[ \t]*\n" nil t)
                  (cond ((looking-at-p "\\[\\[") (setq opening (point)))
                        ((eq (char-before) ?\\) t))))
      (if opening (progn (goto-char opening) (line-beginning-position)) start))))

(defun gptel-ui-links--org-contents-end (opening)
  "Return the end of Org contents allowing multiline links at OPENING."
  (save-match-data
    (save-excursion
      (goto-char opening)
      (let ((element (org-element-at-point)))
        (if (and (memq (org-element-type element) '(paragraph verse-block))
                 (<= (org-element-property :contents-begin element) opening))
            (org-element-property :contents-end element)
          opening)))))

(defun gptel-ui-links--org-brackets (end visit)
  "Call VISIT with the match data for each Org bracket link up to END.
Limit the native grammar to each run of nonblank lines.  Ask Org about
multiline matches since descriptions cannot span element boundaries."
  (while (search-forward "[[" end t)
    (let* ((begin (- (point) 2))
           (paragraph-end (if (re-search-forward "^[ \t]*\n" end t) (point) end))
           (contents-end begin)
           ;; A streamed paragraph may contain many incomplete openers.
           (limit (save-excursion
                    (goto-char paragraph-end)
                    (when (search-backward "]]" begin t) (+ (point) 2)))))
      (when limit
        (goto-char begin)
        (while (re-search-forward org-link-bracket-re limit t)
          (let ((opening (match-beginning 0)))
            (if (or (>= opening (line-beginning-position))
                    (progn
                      (when (<= contents-end opening)
                        (setq contents-end (gptel-ui-links--org-contents-end opening)))
                      (<= (match-end 0) contents-end)))
                (funcall visit)
              ;; A malformed opener can precede another, valid bracket link.
              (goto-char (+ opening 2))))))
      (goto-char paragraph-end))))

(defun gptel-ui-links-render (start end &optional skip-p org-table-cell)
  "Return link overlays for START through END without changing text.
SKIP-P, when non-nil, receives candidate start and end positions; a
non-nil result excludes the link.  ORG-TABLE-CELL means rendering an Org
cell in a temporary buffer, where native links need display actions.
The caller owns the returned overlays."
  (save-excursion
    (let ((org-links (or org-table-cell (derived-mode-p 'org-mode)))
          overlays ranges pending (index []))
      (cl-labels
          ;; Each grammar pass visits nonoverlapping candidates in source
          ;; order.  Only earlier, higher-precedence passes need indexing.
          ;; Merge once per pass, rather than walking every accepted overlay
          ;; for every candidate (quadratic even for ordinary bare URLs).
          ((next-pass ()
             (setq ranges (cl-merge 'list ranges (nreverse pending)
                                    #'< :key #'car)
                   pending nil
                   index (vconcat ranges)))
           (covered (begin finish)
             (or (and skip-p (funcall skip-p begin finish))
                 (gptel-ui-links--overlap-p begin finish index)))
           (decorate (begin finish target label)
             (let ((ov (make-overlay begin finish nil nil nil)))
               (overlay-put ov 'gptel-ui-links t)
               (overlay-put ov 'gptel-ui-link target)
               (overlay-put ov 'face 'gptel-ui-link)
               (overlay-put ov 'mouse-face 'highlight)
               (overlay-put ov 'keymap gptel-ui-links-map)
               (overlay-put ov 'help-echo (concat (gptel-ui-links--target-text target)
                                                  "\nRET or click to open"))
               (overlay-put ov 'priority 30)
               (overlay-put ov 'evaporate t)
               (when label (overlay-put ov 'display (gptel-ui-links-string label target)))
               (push (cons begin finish) pending)
               (push ov overlays)))
           (add-link (begin finish target &optional label existing-only org-bracket)
             (unless (covered begin finish)
               (let* ((destination (gptel-ui-links--destination target))
                      (file (and existing-only
                                 (eq (plist-get destination :type) 'file)
                                 (gptel-ui-links--resolve
                                  (plist-get destination :path)))))
                 (cond
                  ((and org-links org-bracket
                        (not (gptel-ui-links--org-chat-target-p target destination)))
                   ;; Keep unsupported protocols and remote references out
                   ;; of the table chooser.  A custom ID starts with "#"
                   ;; and intentionally has no chat destination.
                   (if (and org-table-cell
                            (or destination (string-prefix-p "#" target)))
                       (decorate begin finish (cons 'org (concat "[[" target "]]")) label)
                     ;; Keep Org's bindings and don't linkify its description.
                     (push (cons begin finish) pending)))
                  ((and destination
                        (or (not existing-only) (and file (file-exists-p file))))
                   (decorate begin finish target label)))))))
        ;; Org links first so their nested brackets cannot become Markdown links.
        (goto-char start)
        (let ((visit (lambda ()
                       (add-link (match-beginning 0) (match-end 0)
                                 (match-string-no-properties 1)
                                 (or (match-string-no-properties 2)
                                     (match-string-no-properties 1)) nil t))))
          (if org-links
              (gptel-ui-links--org-brackets end visit)
            (while (re-search-forward
                    "\\[\\[\\([^]\n]+\\)\\]\\(?:\\[\\([^]\n]*\\)\\]\\)?\\]" end t)
              (funcall visit))))
        (next-pass)
        (goto-char start)
        (while (re-search-forward "\\[\\([^]\n]+\\)\\](" end t)
          (let ((begin (match-beginning 0))
                (label (match-string-no-properties 1))
                (destination-start (point)))
            (when-let* ((finish (gptel-ui-links--closing-paren end)))
              (unless (eq (char-before begin) ?!)
                (add-link begin finish
                          (gptel-ui-links--markdown-target
                           (buffer-substring-no-properties
                            destination-start (1- finish)))
                          (string-trim label "`+" "`+"))))))
        ;; Markdown and Org code-quoted paths, including slashless names.
        (dolist (pattern '("`\\([^`\n]+\\)`" "=\\([^=\n]+\\)="
                           "~\\([^~\n]+\\)~"))
          (next-pass)
          (goto-char start)
          (while (re-search-forward pattern end t)
            (let ((begin (match-beginning 0)) (finish (match-end 0))
                  (target (match-string-no-properties 1)))
              (add-link begin finish target target t))))
        (next-pass)
        (goto-char start)
        (while (re-search-forward "https?://[^[:space:]<>\"'`]+" end t)
          (let* ((begin (match-beginning 0))
                 (target (gptel-ui-links--trim-bare
                          (match-string-no-properties 0))))
            (add-link begin (+ begin (length target)) target)))
        (next-pass)
        (goto-char start)
        (while (re-search-forward
                "\\(?:^\\|[[:space:](]\\)\\(\\(?:/\\|~/\\)[^[:space:]<>\"'`]+\\)" end t)
          (let* ((begin (match-beginning 1))
                 (target (gptel-ui-links--trim-bare
                          (match-string-no-properties 1))))
            (add-link begin (+ begin (length target)) target nil t))))
      (sort overlays (lambda (a b) (< (overlay-start a) (overlay-start b)))))))

(provide 'gptel-ui-links)
;;; gptel-ui-links.el ends here
