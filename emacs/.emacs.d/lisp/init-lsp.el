;;; init-lsp.el --- Configuration for lsp. -*- lexical-binding: t; -*-
;;; Commentary:
;;; General configuration for LSP. Language specific configuration is
;;; handled separately.

;;; Code:

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :commands lsp)

;; lsp extras
(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-sideline-ignore-duplicate t))
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package dap-mode)

(provide 'init-lsp)
;;; init-lsp.el ends here
