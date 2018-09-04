;;; init-ivy.el --- Configuration for SLIME.
;;; Commentary:
;;; SLIME configuration for both SBCL and CCL.

;;; Code:
(use-package slime-company :defer t)

(use-package slime
   :if unix?
   :defer t
   :init
   (setq slime-lisp-implementations
         '((sbcl ("sbcl"))
           (ccl ("ccl"))))
   (setq slime-contribs '(slime-asdf slime-fancy))
   :config
   (slime-setup '(slime-company)))

  ;; (require 'slime-autoloads)
  ;; (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
  ;; (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
  ;; (add-to-list 'load-path (concat-dir "slime/contrib"))
  ;; (setq slime-complete-symbol*-fancy t
  ;;       slime-complete-symbol-function 'slime-fuzzy-complete-symbol
  ;;       common-lisp-hyperspec-root "file:/usr/share/doc/hyperspec/HyperSpec/"
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
