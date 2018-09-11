;;; init-slime.el --- Configuration for SLIME.
;;; Commentary:
;;; SLIME configuration for both SBCL and CCL.

;;; Code:
(use-package slime-company
  :defer t
  :after (company slime))

(use-package slime
  :if unix?
  :defer t
  ;;:hook (lisp-mode . aggressive-indent-mode)
  :hook ((slime-repl-mode . smartparens-strict-mode)
         (slime-repl-mode . rainbow-delimiters-mode)
         (lisp-mode . slime-mode)
         (inferior-lisp-mode . inferior-slime-mode))
  :init
  (setq slime-lisp-implementations
        '((sbcl ("sbcl"))
          (ccl ("ccl"))))
  (setq slime-contribs '(slime-asdf slime-fancy))
  :config
  (slime-setup '(slime-company))
  (add-to-list 'evil-emacs-state-modes 'slime-trace-dialog-mode)
  :custom
  ;; Download HyperSpec from http://ftp.lispworks.com/pub/software_tools/reference/HyperSpec-7-0.tar.gz
  (common-lisp-hyperspec-root (concat "file://" (getenv "HOME") "/Documents/HyperSpec/")))

;; (require 'slime-autoloads)
;; (setq slime-complete-symbol*-fancy t
;;       slime-complete-symbol-function 'slime-fuzzy-complete-symbol
;;       slime-net-coding-system 'utf-8-unix)
;; (add-hook 'slime-mode-hook
;;           (lambda ()
;;             (setq slime-truncate-lines nil)
;;             (slime-redirect-inferior-output)))
;; ;; == Normal SLIME stuff.
;; (global-set-key "\C-cs" 'slime-selector)
;; (global-set-key "\M-i" 'slime-fuzzy-complete-symbol)
;; ;; From http://bc.tech.coop/blog/081209.html
;; (global-set-key "\C-c;" 'slime-insert-balanced-comments)
;; (global-set-key "\C-c\M-;" 'slime-remove-balanced-comments))

(provide 'init-slime)
;;; init-slime.el ends here
