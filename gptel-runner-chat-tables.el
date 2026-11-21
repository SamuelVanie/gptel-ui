;;; gptel-runner-chat-tables.el --- Compatibility loader for gptel-ui-tables -*- lexical-binding: t; -*-

;;; Commentary:

;; The chat view now lives in gptel-ui.  This loader preserves old require
;; forms when the gptel-ui directory is on `load-path'.

;;; Code:

(require 'gptel-ui-tables)

(provide 'gptel-runner-chat-tables)
;;; gptel-runner-chat-tables.el ends here
