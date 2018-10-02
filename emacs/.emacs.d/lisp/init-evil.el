;;; init-evil.el --- Configuration for EVIL and which-key.
;;; Commentary:
;;; Many key bindings follow Spacemacs.

;;; Code:

(use-package evil
  :config
  (evil-set-initial-state 'Info-mode 'emacs))

(use-package evil-leader)
(global-evil-leader-mode)
(evil-mode 1)
(evil-leader/set-leader "<SPC>")

(use-package which-key
  :defer 5
  :diminish
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3)
  )

(evil-leader/set-key
  "<SPC>"    'execute-extended-command
  "ad"       'dired
  "bs"       'switch-to-scratch-buffer
  "bm"       'switch-to-messages-buffer
  "cC"       'compile
  "ck"       'kill-compilation
  "cr"       'recompile
  "fei"      'find-emacs-init-file
  "feR"      'reload-emacs-init-file
  "fg"       'rgrep
  "fs"       'save-buffer
  "hdb"      'describe-bindings
  "hdc"      'describe-char
  "hdk"      'describe-key
  "hdp"      'describe-package
  "jd"       'dired-jump
  "jD"       'dired-jump-other-window
  "hdt"      'describe-theme
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
  "<SPC> j"   "jump/join/split"
  "<SPC> l"   "lisp"
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
