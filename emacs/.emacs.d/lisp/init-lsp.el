;;; init-lsp.el --- Configuration for lsp. -*- lexical-binding: t; -*-
;;; Commentary:
;;; General configuration for LSP. Language specific configuration is
;;; handled separately.

;;; Code:

(use-package lsp-mode
  :config
  ;; make sure we have lsp-imenu everywhere we have LSP
  (require 'lsp-imenu)
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu))

;; lsp extras
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-sideline-ignore-duplicate t))

(use-package company-lsp
  :after lsp-mode
  :config
  (setq company-lsp-enable-snippet t)
  (setq company-lsp-cache-candidates t)
  (push 'company-lsp company-backends))

(provide 'init-lsp)
;;; init-lsp.el ends here
