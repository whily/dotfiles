;;; init-mode-line.el --- Configuration for mode line.
;;; Commentary:
;;; Configuration for moody + minions.

;;; Code:

;; https://github.com/tarsius/moody
(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

;; https://github.com/tarsius/minions
(use-package minions
  :config
  (minions-mode 1)
  :custom
  (minions-direct '(flycheck-mode lsp-mode)))

(provide 'init-mode-line)
;;; init-mode-line.el ends here
