;;; init-window.el --- Configuration for window.
;;; Commentary:
;;; Configuration for winum and eyebrowse.

;;; Code:

;; https://github.com/deb0ch/emacs-winum
(use-package winum
  :config
  (set-face-attribute 'winum-face nil
                      :weight 'bold
                      :foreground "magenta"
                      :background "green")
  (winum-mode)
  (evil-leader/set-key
    "0"        'winum-select-window-0-or-10
    "1"        'winum-select-window-1
    "2"        'winum-select-window-2
    "3"        'winum-select-window-3
    "4"        'winum-select-window-4
    "5"        'winum-select-window-5
    "6"        'winum-select-window-6
    "7"        'winum-select-window-7
    "8"        'winum-select-window-8
    "9"        'winum-select-window-9)
  ;; Group key bindings for winum.
  (push '(("\\(.*\\) 0" . "winum-select-window-0-or-10") . ("\\1 0..9" . "winum-window 0..9-or-10"))
        which-key-replacement-alist)
  (push '((nil . "winum-select-window-[1-9]") . t) which-key-replacement-alist)
  )

;; https://github.com/wasamasa/eyebrowse
;; Keys for evil mode:
;;   gT	Switch to previous window config
;;   gt	Switch to next window config
;;   zx	Switch to last window config
;;   gc	Close current window config
;; `C-c C-w 2' to create and switch to 2nd window config.
;; `C-c C-w ,` to tag the window config.
(use-package eyebrowse
  :ensure t
  :config
  (setq eyebrowse-mode-line-separator " "
        eyebrowse-new-workspace t)
  (eyebrowse-setup-evil-keys)
  (eyebrowse-mode t))

(provide 'init-window)
;;; init-window.el ends here
