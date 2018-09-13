;;; init-expand.el --- Configuration for expand packages.
;;; Commentary:
;;; Configuration for Abbrev, Hippie Expand, YASnippet.

;;; Code:

;; Use abbreviation mode.
(eval-and-compile
  (setq abbrev-file-name "~/.emacs.d/.abbrev_defs"
        save-abbrevs t)
  (dolist (hook '(erc-mode-hook
                  LaTeX-mode-hook
                  prog-mode-hook
                  text-mode-hook))
    (add-hook hook #'abbrev-mode))
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file))
  (diminish 'abbrev-mode))

;; Switch from Dabbrev to Hippie Expand.
(global-set-key [remap dabbrev-expand] 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))

;; Enable YASnippet.
(use-package yasnippet
  :demand t
  :diminish yas-minor-mode
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :config
  (yas-global-mode 1))
(use-package yasnippet-snippets :defer t)

(provide 'init-expand)
;;; init-expand.el ends here
