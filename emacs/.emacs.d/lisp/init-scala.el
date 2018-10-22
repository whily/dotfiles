;;; init-scala.el --- Configuration for Scala. -*- lexical-binding: t; -*-
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

(require 'scala2kt)

(provide 'init-scala)
;;; init-scala.el ends here
