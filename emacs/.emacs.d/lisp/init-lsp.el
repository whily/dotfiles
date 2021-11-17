;;; init-lsp.el --- Configuration for lsp. -*- lexical-binding: t; -*-
;;; Commentary:
;;; General configuration for LSP. Language specific configuration is
;;; handled separately.

;;; Code:

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; lsp extras
(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-sideline-ignore-duplicate t))
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package dap-mode)

;; Minimalist LSP configuration.
(setq lsp-enable-links nil)
(setq lsp-signature-render-documentation nil)
;; headerline breadcrumb shows an icon in headerlin.
;; (setq lsp-headerline-breadcrumb-enable nil)
;; lsp-ui-doc shows object documentation at the point in a child frame.
;; (setq lsp-ui-doc-enable nil)
;; Additional text edits when performing completion.
;; (setq lsp-completion-enable-additional-text-edit nil)
(setq web-mode-enable-current-element-highlight t)

(provide 'init-lsp)
;;; init-lsp.el ends here
