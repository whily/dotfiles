;;; init-scala.el --- Configuration for Scala.
;;; Commentary:
;;; Prefer LSP to Ensime. Metals is not mature at this stage, and
;;; startup time for LSP server is just too long to make it useful.

;;; Code:

;; http://ensime.github.io/editors/emacs/scala-mode/
(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))

;; http://ensime.github.io/editors/emacs/sbt-mode/
;; Start an sbt session with M-x sbt-start or
;;   send a command to the current sbt process with M-x sbt-command
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

;; https://github.com/rossabaker/lsp-scala
(require 'lsp-scala)
(setq lsp-scala-server-command '("metals" "0.1.0-M1+262-3e30bcc2"))
;; Per project setting, in sbt, run following commands: metalsSetup,
;; ~compile (for incremental compiling).
;; For scala file, type `M-x lsp-scala-enable' to start LSP server, and run
;; `M-x lsp-info-under-point' to check whether LSP server has started.

(provide 'init-scala)
;;; init-scala.el ends here
