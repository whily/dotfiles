;;; init-ivy.el --- Configuration for Ivy, counsel, and swiper.
;;; Commentary:
;;; Mainly follows http://oremacs.com/swiper

;;; Code:

(use-package ivy
  :diminish
  :demand t
  :custom
  (ivy-dynamic-exhibit-delay-ms 200)
  (ivy-height 10)
  (ivy-initial-inputs-alist nil t)
  (ivy-magic-tilde nil)
  (ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (ivy-wrap t)
  :config
  (ivy-mode 1))

(use-package counsel
  :after ivy
  :demand t
  :diminish
  :commands counsel-git-grep
  :bind ("C-c g" . counsel-git-grep)
  :config
  (counsel-mode 1))

(use-package counsel-projectile
  :after (counsel projectile)
  :config (counsel-projectile-mode 1))

(use-package swiper
  :bind ("C-s" . swiper))

(provide 'init-ivy)
;;; init-ivy.el ends here
