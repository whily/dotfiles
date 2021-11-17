;;; init-json.el --- Configuration for JSON. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Configuration for JSON.

;;; Code:

;; Install jsonlint
;;    npm install jsonlint -g

;; Flycheck uses jsonlint to report errors.
(add-hook 'json-mode-hook #'flycheck-mode)

;; Format json code by function `json-pretty-print-buffer'

(provide 'init-json)
;;; init-json.el ends here
