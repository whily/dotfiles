;;; init-kotlin.el --- Configuration for Kotlin.
;;; Commentary:
;;; Configuration for Kotlin and Gradle.

;;; Code:

;; https://github.com/Emacs-Kotlin-Mode-Maintainers
(use-package kotlin-mode
  :mode "\\.kts?\\'")

(use-package gradle-mode
  :config
  (gradle-mode 1))

(require 'lsp-kotlin)
;; TODO: it seems below does not work.
(add-hook 'kotlin-mode #'lsp-kotlin-enable)

(provide 'init-kotlin)
;;; init-kotlin.el ends here
