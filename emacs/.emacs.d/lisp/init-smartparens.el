;;; init-smartparens.el --- Configuration for smartparens.
;;; Commentary:
;;; Configuration for smartparens according to:
;;;    https://github.com/Fuco1/smartparens
;;; Code:

(use-package smartparens
  :diminish
  :hook ((lisp-mode emacs-lisp-mode) . smartparens-strict-mode)
  :config
  (require 'smartparens-config)
  )

(provide 'init-smartparens)
;;; init-smartparens.el ends here
