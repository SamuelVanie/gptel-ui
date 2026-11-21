;;; gptel-runner-chat.el --- Compatibility loader for gptel-ui -*- lexical-binding: t; -*-

;;; Commentary:

;; The chat view now lives in gptel-ui.  This loader preserves old require
;; forms when the gptel-ui directory is on `load-path'.

;;; Code:

(require 'gptel-ui)

(provide 'gptel-runner-chat)
;;; gptel-runner-chat.el ends here
