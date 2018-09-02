;;; init-ivy.el --- Configuration for EVIL and which-key.
;;; Commentary:
;;; Many key bindings follow Spacemacs.

;;; Code:

(use-package evil)
(use-package evil-leader)
(global-evil-leader-mode)
(evil-mode 1)
(evil-leader/set-leader "<SPC>")

(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3))

(evil-leader/set-key
  "<SPC>"    'execute-extended-command
  "fb"       'counsel-bookmark
  "fei"      'find-emacs-init-file
  "feR"      'reload-emacs-init-file
  "ff"       'counsel-find-file
  "fL"       'counsel-locate
  "fr"       'counsel-locate
  "fs"       'save-buffer
  "hdb"      'describe-bindings
  "hdc"      'describe-char
  "hdf"      'describe-function
  "hdk"      'describe-key
  "hdv"      'describe-variable)

(which-key-add-key-based-replacements
 "<SPC> f"   "files"
 "<SPC> f e" "emacs"
 "<SPC> h"   "help"
 "<SPC> h d" "help-describe")

(defun find-emacs-init-file ()
  "Edit Emacs init.el in the current window."
  (interactive)
  (find-file-existing user-init-file))

(defun reload-emacs-init-file ()
  "Reload Emacs init.el."
  (interactive)
  (load-file user-init-file))

(provide 'init-evil)
;;; init-evil.el ends here
