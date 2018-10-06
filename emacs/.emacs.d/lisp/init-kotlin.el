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

;;;###autoload
(defcustom lsp-kotlin-server
  "kotlin-language-server" ;"~/pkg/KotlinLanguageServer/build/install/kotlin-language-server/bin/kotlin-language-server"
  "The kotlin executable to use.
Leave as just the executable name to use the default behavior of
finding the executable with `exec-path'.
   TODO: now we use full path to allow the find of JAR files in ../lib"
  :group 'lsp-kotlin
  :risky t
  :type 'file)

;;;###autoload
(defcustom lsp-kotlin-server-args
  '()
  "Extra arguments for the kotlinstdio language server"
  :group 'lsp-kotlin
  :risky t
  :type '(repeat string))

(defconst lsp-kotlin--get-root
  (lsp-make-traverser #'(lambda (dir)
			  (directory-files dir nil "build.gradle"))))

(defun lsp-kotlin--ls-command ()
  "Generate the language server startup command."
  `(,lsp-kotlin-server
    ,@lsp-kotlin-server-args))

(lsp-define-stdio-client
 lsp-kotlin "kotlin"
 lsp-kotlin--get-root
 nil
 :command-fn 'lsp-kotlin--ls-command)
;; TODO: it seems below does not work.
(add-hook 'kotlin-mode #'lsp-kotlin-enable)

(provide 'init-kotlin)
;;; init-kotlin.el ends here
