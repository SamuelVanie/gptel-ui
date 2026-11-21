;;; gptel-runner-chat-tools.el --- Compatibility loader for gptel-ui-tools -*- lexical-binding: t; -*-

;;; Commentary:

;; The chat view now lives in gptel-ui.  This loader preserves old require
;; forms when the gptel-ui directory is on `load-path'.

;;; Code:

(require 'gptel-ui-tools)

(provide 'gptel-runner-chat-tools)
;;; gptel-runner-chat-tools.el ends here
