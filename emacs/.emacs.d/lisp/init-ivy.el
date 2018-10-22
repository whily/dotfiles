;;; init-ivy.el --- Configuration for Ivy, counsel, and swiper. -*- lexical-binding: t; -*-
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
  :config
  (counsel-mode 1))

(evil-leader/set-key
  "fb"       'counsel-bookmark
  "ff"       'counsel-find-file
  "fL"       'counsel-locate
  "fr"       'counsel-recentf
  "hdf"      'counsel-describe-function
  "hdv"      'counsel-describe-variable
  "iu"       'counsel-unicode-char
  "ji"       'counsel-imenu
  "pg"       'counsel-git-grep
  "ry"       'counsel-yank-pop
  "Ts"       'counsel-load-theme
  )

(use-package counsel-projectile
  :after (counsel projectile)
  :config (counsel-projectile-mode 1))

(evil-leader/set-key
  "p <SPC>"  'counsel-projectile
  "pb"       'counsel-projectile-switch-to-buffer
  "pd"       'counsel-projectile-find-dir
  "pf"       'counsel-projectile-find-file
  "pp"       'counsel-projectile-switch-project
  "pR"       'counsel-projectile-rg
  )

(use-package swiper
  :bind ("C-s" . swiper))

(evil-leader/set-key
  "sb"       'swiper-all
  "ss"       'swiper
  )

(provide 'init-ivy)
;;; init-ivy.el ends here
