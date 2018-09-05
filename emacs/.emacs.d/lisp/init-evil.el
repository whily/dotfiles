;;; init-ivy.el --- Configuration for EVIL and which-key.
;;; Commentary:
;;; Many key bindings follow Spacemacs.

;;; Code:

(use-package evil)
(use-package evil-leader)
(global-evil-leader-mode)
(evil-mode 1)
(evil-leader/set-leader "<SPC>")
(add-to-list 'evil-emacs-state-modes 'slime-trace-dialog-mode)

(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3)
  ;; Group key bindings for winum.
  (push '(("\\(.*\\) 0" . "winum-select-window-0-or-10") . ("\\1 0..9" . "winum-window 0..9-or-10"))
        which-key-replacement-alist)
  (push '((nil . "winum-select-window-[1-9]") . t) which-key-replacement-alist))

(evil-leader/set-key
  "<SPC>"    'execute-extended-command
  "0"        'winum-select-window-0-or-10
  "1"        'winum-select-window-1
  "2"        'winum-select-window-2
  "3"        'winum-select-window-3
  "4"        'winum-select-window-4
  "5"        'winum-select-window-5
  "6"        'winum-select-window-6
  "7"        'winum-select-window-7
  "8"        'winum-select-window-8
  "9"        'winum-select-window-9
  "bs"       'switch-to-scratch-buffer
  "bm"       'switch-to-messages-buffer
  "en"       'flycheck-next-error
  "ep"       'flycheck-previous-error
  "fb"       'counsel-bookmark
  "fei"      'find-emacs-init-file
  "feR"      'reload-emacs-init-file
  "ff"       'counsel-find-file
  "fg"       'rgrep
  "fL"       'counsel-locate
  "fr"       'counsel-recentf
  "fs"       'save-buffer
  "hdb"      'describe-bindings
  "hdc"      'describe-char
  "hdf"      'counsel-describe-function
  "hdk"      'describe-key
  "hdp"      'describe-package
  "hdt"      'describe-theme
  "hdv"      'counsel-describe-variable
  "iu"       'counsel-unicode-char
  "p <SPC>"  'counsel-projectile
  "pb"       'counsel-projectile-switch-to-buffer
  "pd"       'counsel-projectile-find-dir
  "pf"       'counsel-projectile-find-file
  "pp"       'counsel-projectile-switch-project
  "pr"       'projectile-recentf
  "ry"       'counsel-yank-pop
  "sb"       'swiper-all
  "sj"       'counsel-imenu
  "ss"       'swiper
  "Ts"       'counsel-load-theme
  "wo"       'other-frame
  "ws"       'split-window-below
  "wv"       'split-window-right
  "w="       'balance-windows
)

(which-key-add-key-based-replacements
 "<SPC> a"   "applications"
 "<SPC> a i" "irc"
 "<SPC> a s" "shells"
 "<SPC> b"   "buffers"
 "<SPC> c"   "compile/comments"
 "<SPC> e"   "errors"
 "<SPC> f"   "files"
 "<SPC> f e" "emacs"
 "<SPC> g"   "git/version-control"
 "<SPC> h"   "help"
 "<SPC> h d" "help-describe"
 "<SPC> i"   "insertion"
 "<SPC> p"   "projects"
 "<SPC> r"   "registers/rings/resume"
 "<SPC> s"   "search/symbol"
 "<SPC> T"   "themes"
 "<SPC> w"   "windows")

(defun find-emacs-init-file ()
  "Edit Emacs init.el in the current window."
  (interactive)
  (find-file-existing user-init-file))

(defun switch-to-scratch-buffer ()
  "Switch to *scratch* buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*")))

(defun switch-to-messages-buffer ()
  "Switch to *Messages* buffer."
  (interactive)
  (switch-to-buffer (get-buffer "*Messages*")))

(defun reload-emacs-init-file ()
  "Reload Emacs init.el."
  (interactive)
  (load-file user-init-file))

(provide 'init-evil)
;;; init-evil.el ends here
