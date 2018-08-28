;;; init-elpa.el --- Configuration for ELPA package management.

;; ELPA package management. Main reference: https://www.emacswiki.org/emacs/ELPA
;; Mirror in China: use https://github.com/emacs-china/elpa in addition to popkit.
;; Another mirror, not tested, not added below: https://mirrors4.tuna.tsinghua.edu.cn/help/elpa/
(require 'package)
(setq package-archives
      '(("gnu-elpa"     . "http://elpa.emacs-china.org/gnu/")
        ("melpa-stable" . "http://elpa.emacs-china.org/melpa-stable/")
        ("melpa" .        "http://elpa.emacs-china.org/melpa/")
        ("org"          . "https://orgmode.org/elpa/"))
      package-archive-priorities
      '(("melpa-stable" . 10)
        ("gnu-elpa"     . 4)
        ("melpa"        . 2)
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
(require 'use-package)

;; Automatically update packages.
(use-package auto-package-update)

(provide 'init-elpa)
