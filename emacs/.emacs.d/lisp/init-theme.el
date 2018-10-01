;;; init-theme.el --- Configuration for themes.
;;; Commentary:
;;; Refer to link below for popular Emacs themes:
;;;   https://pawelbx.github.io/emacs-theme-gallery/

;;; Code:

(use-package material-theme                 :defer t)
(use-package atom-one-dark-theme            :defer t)
(use-package monokai-theme                  :defer t)
(use-package zenburn-theme                  :defer t)
(use-package color-theme-sanityinc-tomorrow :defer t)
(use-package solarized-theme)
;; Put dracula at the end to make it the default theme loaded.
(use-package dracula-theme
  :config
  (load-theme 'dracula t)
  ;; According to https://github.com/tarsius/moody
  ;; Only configure this once (not for other themes as they should be
  ;; be automatically applied).
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :background "#d9d2c9")))

(setq themes '(dracula material atom-one-dark monokai zenburn
                       sanityinc-tomorrow-day solarized-light))
(setq theme-index 0)
(defun cycle-theme ()
  "Cycle throuhg available themes."
  (interactive)
  (setq theme-index (mod (1+ theme-index) (length themes)))
  (let ((theme (nth theme-index themes)))
    (load-theme theme t)
    (message "Load theme: %s." theme)))
(evil-leader/set-key "Tn" 'cycle-theme)

(provide 'init-theme)
;;; init-theme.el ends here
