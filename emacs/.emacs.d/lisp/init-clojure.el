;;; init-clojure.el --- Configuration for clojure.
;;; Commentary:
;;; Clojure configuration.

;;; Code:

;; https://github.com/clojure-emacs/clojure-mode/
(use-package clojure-mode
  :hook ((clojure-mode . smartparens-strict-mode)
         (clojure-mode . aggressive-indent-mode)))

(use-package clj-refactor
  :disabled t
  :defer t
  :after clojure-mode
  :config
  (defun my-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1)      ; for adding require/use/import statements
    ;; This choice of keybinding leaves cider-macroexpand-1 unbound
    (cljr-add-keybindings-with-prefix "C-c C-m"))
  (add-hook 'clojure-mode-hook #'my-clojure-mode-hook))

;; Cider for clojure.
;; Install first.
(use-package cider
  :defer t
  ;; Configuration according to https://github.com/clojure-emacs/cider
  ;; Enable eldoc in Clojure buffer
  :hook ((cider-mode . cider-turn-on-eldoc-mode)
         (cider-mode . smartparens-strict-mode)
         (cider-mode . rainbow-delimiters-mode))
  :custom
  ;; Hide the *nrepl-connection* and *nrepl-server* buffers from
  ;; appearing in some buffer switching commands
  (nrepl-hide-special-buffers t)
  ;; Prevent the auto-display of the REPL buffer in a separate window
  ;; after connection is established.
  (cider-repl-pop-to-buffer-on-connect nil)
  ;; Stop the error buffer from popping up while working in buffers
  ;; other than the REPL.
  (cider-popup-stacktraces nil)
  ;; Enable error buffer popping also in the REPL.
  (cider-repl-popup-stacktraces t)
  ;; To auto-select the error buffer when it's displayed.
  (cider-auto-select-error-buffer t)
  :config
  (add-to-list 'evil-emacs-state-modes 'cider-docview-mode))

(provide 'init-clojure)
;;; init-clojure.el ends here
