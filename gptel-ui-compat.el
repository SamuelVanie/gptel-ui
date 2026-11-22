;;; gptel-ui-compat.el --- Compatibility for the former chat view -*- lexical-binding: t; -*-

;;; Commentary:

;; Keep existing public chat commands, settings and faces working after the
;; move from gptel-runner.  Load aliases before the new definitions so values
;; configured under the old names survive initialization.  No runner library
;; is required.  The implementation and its private state belong to gptel-ui.

;;; Code:

(declare-function gptel-ui-toggle-report "gptel-ui" (&optional event))
(declare-function gptel-ui-refresh "gptel-ui" (&rest _))
(declare-function gptel-ui-toggle-source "gptel-ui" ())
(declare-function gptel-ui-next-turn "gptel-ui" (&optional previous))
(declare-function gptel-ui-previous-turn "gptel-ui" ())
(declare-function gptel-ui-mode "gptel-ui" (&optional arg))
(declare-function gptel-ui-global-mode "gptel-ui" (&optional arg))
(declare-function gptel-ui-demo "gptel-ui" ())
(declare-function gptel-ui-links-open "gptel-ui-links" (&optional event))
(declare-function gptel-ui-links-string "gptel-ui-links" (label target))
(declare-function gptel-ui-links-render "gptel-ui-links" (start end &optional skip-p org-table-cell))
(declare-function gptel-ui-tables-render "gptel-ui-tables" (start end &optional skip-p))
(declare-function gptel-ui-tools-toggle "gptel-ui-tools" (&optional event))
(declare-function gptel-ui-tools-render-block "gptel-ui-tools" (beg opening-end body-beg close-beg end info))

(defun gptel-ui-compat--variable-alias (old new)
  "Alias OLD variable to NEW, preserving settings saved before loading.
An explicit customization of NEW takes precedence over the old name."
  (dolist (property '(saved-value saved-variable-comment customized-value
                     theme-value))
    (when (and (get old property) (not (get new property)))
      (put new property (get old property))))
  (defvaralias old new))

(gptel-ui-compat--variable-alias 'gptel-runner-chat-width 'gptel-ui-width)
(gptel-ui-compat--variable-alias 'gptel-runner-chat-refresh-delay 'gptel-ui-refresh-delay)
(gptel-ui-compat--variable-alias 'gptel-runner-chat-assistant-name 'gptel-ui-assistant-name)
(gptel-ui-compat--variable-alias 'gptel-runner-chat-mode 'gptel-ui-mode)
(gptel-ui-compat--variable-alias 'gptel-runner-chat-mode-map 'gptel-ui-mode-map)
(gptel-ui-compat--variable-alias 'gptel-runner-chat-links-map 'gptel-ui-links-map)
(gptel-ui-compat--variable-alias 'gptel-runner-chat-table-width 'gptel-ui-table-width)
(gptel-ui-compat--variable-alias 'gptel-runner-chat-tools-map 'gptel-ui-tools-map)
(gptel-ui-compat--variable-alias 'gptel-runner-chat-global-mode 'gptel-ui-global-mode)
(gptel-ui-compat--variable-alias 'gptel-runner-chat-mode-hook 'gptel-ui-mode-hook)
(gptel-ui-compat--variable-alias 'gptel-runner-chat-mode-on-hook 'gptel-ui-mode-on-hook)
(gptel-ui-compat--variable-alias 'gptel-runner-chat-mode-off-hook 'gptel-ui-mode-off-hook)
(gptel-ui-compat--variable-alias 'gptel-runner-chat-global-mode-hook 'gptel-ui-global-mode-hook)
(gptel-ui-compat--variable-alias 'gptel-runner-chat-global-mode-on-hook 'gptel-ui-global-mode-on-hook)
(gptel-ui-compat--variable-alias 'gptel-runner-chat-global-mode-off-hook 'gptel-ui-global-mode-off-hook)

(define-obsolete-function-alias 'gptel-runner-chat-toggle-report
  #'gptel-ui-toggle-report "0.1.0")
(define-obsolete-function-alias 'gptel-runner-chat-refresh
  #'gptel-ui-refresh "0.1.0")
(define-obsolete-function-alias 'gptel-runner-chat-toggle-source
  #'gptel-ui-toggle-source "0.1.0")
(define-obsolete-function-alias 'gptel-runner-chat-next-turn
  #'gptel-ui-next-turn "0.1.0")
(define-obsolete-function-alias 'gptel-runner-chat-previous-turn
  #'gptel-ui-previous-turn "0.1.0")
(define-obsolete-function-alias 'gptel-runner-chat-mode
  #'gptel-ui-mode "0.1.0")
(define-obsolete-function-alias 'gptel-runner-chat-global-mode
  #'gptel-ui-global-mode "0.1.0")
(define-obsolete-function-alias 'gptel-runner-chat-demo
  #'gptel-ui-demo "0.1.0")
(define-obsolete-function-alias 'gptel-runner-chat-links-open
  #'gptel-ui-links-open "0.1.0")
(define-obsolete-function-alias 'gptel-runner-chat-links-string
  #'gptel-ui-links-string "0.1.0")
(define-obsolete-function-alias 'gptel-runner-chat-links-render
  #'gptel-ui-links-render "0.1.0")
(define-obsolete-function-alias 'gptel-runner-chat-tables-render
  #'gptel-ui-tables-render "0.1.0")
(define-obsolete-function-alias 'gptel-runner-chat-tools-toggle
  #'gptel-ui-tools-toggle "0.1.0")
(define-obsolete-function-alias 'gptel-runner-chat-tools-render-block
  #'gptel-ui-tools-render-block "0.1.0")

(defun gptel-ui-compat--face-alias (old new)
  "Alias OLD face to NEW, preserving customization made before loading.
An explicit customization of NEW takes precedence over the old name."
  (dolist (property '(saved-face saved-face-comment customized-face theme-face))
    (when (and (get old property) (not (get new property)))
      (put new property (get old property))))
  (define-obsolete-face-alias old new "0.1.0"))

(gptel-ui-compat--face-alias 'gptel-runner-chat-user 'gptel-ui-user)
(gptel-ui-compat--face-alias 'gptel-runner-chat-assistant 'gptel-ui-assistant)
(gptel-ui-compat--face-alias 'gptel-runner-chat-heading 'gptel-ui-heading)
(gptel-ui-compat--face-alias 'gptel-runner-chat-subheading 'gptel-ui-subheading)
(gptel-ui-compat--face-alias 'gptel-runner-chat-muted 'gptel-ui-muted)
(gptel-ui-compat--face-alias 'gptel-runner-chat-code 'gptel-ui-code)
(gptel-ui-compat--face-alias 'gptel-runner-chat-code-label 'gptel-ui-code-label)
(gptel-ui-compat--face-alias 'gptel-runner-chat-inline-code 'gptel-ui-inline-code)
(gptel-ui-compat--face-alias 'gptel-runner-chat-report 'gptel-ui-report)
(gptel-ui-compat--face-alias 'gptel-runner-chat-quote 'gptel-ui-quote)
(gptel-ui-compat--face-alias 'gptel-runner-chat-link 'gptel-ui-link)
(gptel-ui-compat--face-alias 'gptel-runner-chat-table 'gptel-ui-table)
(gptel-ui-compat--face-alias 'gptel-runner-chat-table-header 'gptel-ui-table-header)
(gptel-ui-compat--face-alias 'gptel-runner-chat-table-border 'gptel-ui-table-border)
(gptel-ui-compat--face-alias 'gptel-runner-chat-tool-header 'gptel-ui-tool-header)
(gptel-ui-compat--face-alias 'gptel-runner-chat-tool-body 'gptel-ui-tool-body)
(gptel-ui-compat--face-alias 'gptel-runner-chat-tool-label 'gptel-ui-tool-label)

(provide 'gptel-ui-compat)
;;; gptel-ui-compat.el ends here
