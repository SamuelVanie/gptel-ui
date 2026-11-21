;;; gptel-runner-chat-links.el --- Compatibility loader for gptel-ui-links -*- lexical-binding: t; -*-

;;; Commentary:

;; The chat view now lives in gptel-ui.  This loader preserves old require
;; forms when the gptel-ui directory is on `load-path'.

;;; Code:

(require 'gptel-ui-links)

(provide 'gptel-runner-chat-links)
;;; gptel-runner-chat-links.el ends here
