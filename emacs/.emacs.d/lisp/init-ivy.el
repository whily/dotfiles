;;; init-ivy.el --- Configuration for Ivy, counsel, and swiper.
;;; Commentary:
;;; Mainly follows http://oremacs.com/swiper

;;; Code:

(use-package counsel
  :commands counsel-git-grep
  :bind ("C-c g" . counsel-git-grep)
  :demand t
  :config
  (ivy-mode 1)
  (counsel-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "))

(use-package swiper
  :bind ("C-s" . swiper))

(provide 'init-ivy)
;;; init-ivy.el ends here
