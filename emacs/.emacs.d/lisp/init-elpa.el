;;; init-elpa.el --- Configuration for ELPA package management. -*- lexical-binding: t; -*-
;;; Commentary:
;;; The repos are mainly from mirros in China.

;;; Code:

;; ELPA package management. Main reference: https://www.emacswiki.org/emacs/ELPA
;; Mirror in China: use https://github.com/emacs-china/elpa in addition to popkit.
(require 'package)
(setq package-archives
      '(("gnu-elpa"     . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/stable-melpa/")
        ("melpa" .        "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ("org"          . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/"))
      package-archive-priorities
      '(("melpa-stable" . 8)
        ("gnu-elpa"     . 4)
        ("melpa"        . 10)
        ("org"          . 1)))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
;; To keep up-to-date, do `M-x list-packages RET U x RET`, or
;;   use `M-x auto-package-update-now`.
;; To delete a package, do `M-x list-packages RET d x RET`, or
;;   use `M-x package-delete RET`.

;; Bootstrap use-package: https://github.com/jwiegley/use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents) (package-install 'use-package))
(setq use-package-always-ensure t)
(eval-when-compile
  (require 'use-package)
  (require 'bind-key))

;; Automatically update packages.
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (setq auto-package-update-interval 28)
  (auto-package-update-maybe))

(use-package use-package-ensure-system-package
  :ensure t)

(use-package diminish :defer t)

(provide 'init-elpa)
;;; init-elpa.el ends here
