;;; init.el --- My emacs configuration file.
;;; Commentary:
;;; With reference to configuration from purcell and spacemacs.

;;; Code:

;; Emacs load time below and at the end of the file, as well as
;; parameter tuning (which reduces startup time significantly)
;; are based on
;;    https://github.com/jwiegley/dot-emacs/blob/master/init.el
(defconst emacs-start-time (current-time))

(defvar file-name-handler-alist-old file-name-handler-alist)

(setq package-enable-at-startup nil
      file-name-handler-alist nil
      message-log-max 16384
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      auto-window-vscroll nil)

(add-hook 'after-init-hook
          `(lambda ()
             (setq file-name-handler-alist file-name-handler-alist-old
                   gc-cons-threshold 800000
                   gc-cons-percentage 0.1)
             (garbage-collect)) t)

;; For debug purpose. When encoutering an EmacsLisp error, this will
;; pop up a BacktraceBuffer.
;; Disabled for now as it is boring.
;;(setq debug-on-error t)

;;; Environment.

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; VC follows the symbolic links and visits the real file, without
;; asking for confirmation.
(setq vc-follow-symlinks t)

(require 'init-elpa)

;;; Libraries.

;; As a fan of Common Lisp.
(require 'cl)

(use-package dash          :defer t)

;;; ----------------------- General ----------------------------

;; Smart mode line: https://github.com/Malabarba/smart-mode-line/
(use-package smart-mode-line
  :config
  (setq sml/no-confirm-load-theme t)
  (sml/setup)
  (sml/apply-theme 'dark))

;; Diminish minor modes not installed by use-package.
(diminish 'abbrev-mode)

(use-package dashboard
  :config
  (dashboard-setup-startup-hook))

(use-package rainbow-mode
  :commands rainbow-mode)


(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(require 'init-evil)

;; Start emacs in full screen. Note that -mm option is not available for emacsclient.
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Detect window system.
(setq windows? (string-equal system-type "windows-nt"))
(setq linux? (string-equal system-type "gnu/linux"))
(setq osx? (string-equal system-type "darwin"))
(setq unix? (or linux? osx?))

;; C-SPACE is occupied by IME, so use C-return instead.
(define-key global-map [C-return] 'set-mark-command)

;; Enable the interaction with system clipboard.
(setq x-select-enable-clipboard t)

;; Remove the startup message.
(setq inhibit-startup-message t)

;; Disable backup files.
(setq make-backup-files nil)

;; Disable lock files
(setq create-lockfiles nil)

;; Alwyas move to the beginning or the end of the buffer.
(setq scroll-error-top-bottom t)

;; Always end a file with a newline.
(setq require-final-newline t)

;; No trailing whitespace.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Single space ends a sentence (relevant for filling).
(setq sentence-end-double-space nil)

;; Load material theme
;(use-package material-theme
;  :config
;  (load-theme 'material t))
(use-package dracula-theme
  :config
  (load-theme 'dracula t))
;(load-theme 'solarized-dark t)

;; Enable line numbers globally
(global-linum-mode t)

;; More room between line number and actual content.
(fringe-mode 22)

;; Do not indent tabs. NOTE: may break for makefile or shell scripts!
(setq-default indent-tabs-mode nil)

(require 'init-ivy)

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
    "9"        'winum-select-window-9))

;; avy https://github.com/abo-abo/avy
;; Maybe try https://github.com/tam17aki/ace-isearch ?
(use-package avy
  :config
  (avy-setup-default)
  :bind ("C-c j" . avy-goto-word-or-subword-1))

;; Copy configuration from https://github.com/jwiegley/use-package
(use-package color-moccur
  :commands (isearch-moccur isearch-all isearch-moccur-all)
  :bind (("M-s O" . moccur)
         :map isearch-mode-map
         ("M-o" . isearch-moccur)
         ("M-O" . isearch-moccur-all))
  :init
  (setq isearch-lazy-highlight t)
  :config
  (use-package moccur-edit))

;; https://github.com/wasamasa/nov.el
(use-package nov
  :mode ("\\.epub\\'" . nov-mode))

;; When splitting windows, prefer to split horizontally, as in
;; http://stackoverflow.com/questions/2081577/setting-emacs-split-to-horizontal
(setq split-height-threshold nil)
(setq split-width-threshold 40)

;; Setup Emacs to run bash as its primary shell.
(setq shell-file-name "bash")
(setq shell-command-switch "-c")
(setq explicit-shell-file-name shell-file-name)
(setenv "SHELL" shell-file-name)
(setq explicit-bash-args '("-login" "-i"))
(setenv "PAGER" "/bin/cat")
(setenv "EDITOR" "/usr/bin/emacsclient")

;; Enable Org mode.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
; If there are wrong characters shown in Org deadline, Enable the following line.
; (setq system-time-locale "C")
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-todo-keywords
      '((sequence "TODO" "STARTED" "WAITING" "|" "DONE" "DELEGATED")))
(setq org-todo-keyword-faces
      '(("WAITING" . (:foreground "yellow" :weight bold))
        ("STARTED" . (:foreground "purple" :weight bold))
        ("DELEGATED" . (:foreground "cyan" :weight bold))))
(setq org-agenda-files (file-expand-wildcards "~/org/*.org"))
(setq org-log-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-start-on-weekday nil)
(setq org-reverse-note-order t)
(defun gtd ()
  (interactive)
  (find-file "~/org/task.org"))
;; Set the LaTeX preview progam. It seems that imagemagic does not work.
(setq org-latex-create-formula-image-program 'imagemagick)
;; Org capture for journal.
(setq my-org-dir "~/org/")
(define-key global-map "\C-cc" 'org-capture)
(setq org-capture-templates
      (list
       ;; Journal
       (list "j" "Journal" 'entry
             (list 'file+datetree+prompt (concat my-org-dir "/journal.org"))
             "* %?\n  -- %T\n")
       ))

;;; ------------------- Change keybindings ---------------------

;; Replace Alt-x with C-x C-m
(global-set-key "\C-x\C-m" 'execute-extended-command)

;; Unset C-xm, which can be errorly pressed for C-xC-m.
(global-unset-key "\C-xm")

;; Bind backward-kill-word and rebind kill-region.
(global-set-key "\C-w" 'backward-kill-word)

;; Customize "\C-hm" for widescreen laptops.
(fset 'my-describe-mode [?\C-x ?1 ?\C-x ?3 ?\M-x ?d ?e ?s ?c ?r
                               ?i ?b ?e ?- ?m ?o ?d ?e return ?\C-x ?o])
(global-set-key "\C-hm" 'my-describe-mode)

;;; ----------------------- Apperance --------------------------

;; Disable menu-bar, tool-bar and scroll-bar.
(menu-bar-mode -1)
(tool-bar-mode 0)
(scroll-bar-mode -1)

;; Always show column number.
(setq column-number-mode t)

;; Display time.
(display-time)

;; https://github.com/rolandwalker/unicode-fonts
(use-package unicode-fonts
  :init
  ;; Minimize the startup delay due to font addition/deletion etc.
  (setq unicode-fonts-block-font-mapping
        '(("CJK Compatibility" ("Source Han Sans CN"))
          ("CJK Compatibility Forms" ("Source Han Sans CN"))
          ("CJK Compatibility Ideographs" ("Source Han Sans CN"))
          ("CJK Compatibility Ideographs Supplement" ("Source Han Sans CN"))
          ("CJK Radicals Supplement" ("Source Han Sans CN"))
          ("CJK Strokes" ("Source Han Sans CN"))
          ("CJK Symbols and Punctuation" ("Source Han Sans CN"))
          ("CJK Unified Ideographs" ("Source Han Sans CN"))
          ("CJK Unified Ideographs Extension A" ("Source Han Sans CN"))
          ("CJK Unified Ideographs Extension B" ("Source Han Sans CN"))
          ("CJK Unified Ideographs Extension C" ("Source Han Sans CN"))
          ("CJK Unified Ideographs Extension D" ("Source Han Sans CN"))
          ("CJK Unified Ideographs Extension E" ("Source Han Sans CN"))
          ("CJK Unified Ideographs Extension F" ("Source Han Sans CN")))
        unicode-fonts-fontset-names '("fontset-default")))
(unicode-fonts-setup)

;; Set font.
(set-frame-font "Inconsolata 18" nil t)

;; Highlight selected region.
(setq transient-mark-mode t)

;; Disable Electric Indent mode.
(electric-indent-mode 0)

;; Setup Ediff to use one frame only, which is useful for Stumpwm.
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;; ------------------- Development tools -----------------------

;; Projectile from https://github.com/bbatsov/projectile
;; To manually mark a project folder (which is not version controlled),
;; create file named .projectile
(use-package projectile
  :defer 5
  :diminish
  :config
  (projectile-global-mode)
  (evil-leader/set-key
    "pr"       'projectile-recentf))

(require 'init-paredit)

;; Zeal at point: https://github.com/jinzhu/zeal-at-point
(use-package zeal-at-point
  :bind ("\C-cd" . zeal-at-point)
  :config
  (add-to-list 'zeal-at-point-mode-alist '(java-mode . "java"))
  (add-to-list 'zeal-at-point-mode-alist '(python-mode . "python"))
  (add-to-list 'zeal-at-point-mode-alist '(scala-mode . ("scala" "java"))))

;; Turn on auto fill mode for text.
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Configure Ediff.
(setq ediff-split-window-function 'split-window-horizontally)

;; NASM mode.
(use-package nasm-mode
  :mode "\\.\\(asm\\|s\\)$"
  :config
  (add-hook 'nasm-mode-hook
            (lambda () (setq-default nasm-basic-offset 4))))

;; MMIX mode.
(autoload 'mmix-mode "mmix-mode" "Major mode for editing MMIX files" t)
(setq auto-mode-alist (cons '("\\.mms" . mmix-mode)
                            auto-mode-alist))

;; Markdown mode from http://jblevins.org/projects/markdown-mode/
;; Although every md file is handled in gfm-mode, the flexible
;; configuration from the example is kept as is.
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "markdown"))

;; Based on https://github.com/jwiegley/dot-emacs/blob/master/init.el
(use-package markdown-preview-mode
  :after markdown-mode
  :config
  (setq markdown-preview-stylesheets
        (list (concat "https://github.com/dmarcotte/github-markdown-preview/"
                      "blob/master/data/css/github.css"))))
;; Magit
(use-package magit
  :commands magit-status magit-blame
  :init (setq
         magit-revert-buffers nil)
  :bind (("s-g" . magit-status)
         ("s-b" . magit-blame)))
;; For introduction of magit, follow https://masteringemacs.org/article/introduction-magit-emacs-mode-git
;; Useful commands:
;;   ?    show available commands
;;   RET  go to the file where the change is made
;;   s or u to stage/unstage the item (file, hunk, or selected region)
;;   c a  amend commit
;;   c r  reword commit message
;;   l l  short log
;;   l h  reflog
;;   l x  reset head to the selected commit
;;   l v  revert the commit

;; Make keychain environent available to Emacs (especially Magit).
(use-package keychain-environment)
(keychain-refresh-environment)

;; Enable YASnippet.
(use-package yasnippet
  :demand t
  :diminish yas-minor-mode
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :config
  (yas-global-mode 1))
(use-package yasnippet-snippets :defer t)

(use-package zoom
  :bind ("C-x +" . zoom)
  :preface
  (defun size-callback ()
    (cond ((> (frame-pixel-width) 1280) '(90 . 0.75))
          (t '(0.5 . 0.5)))))

;; Sage whell mode.
(unless (package-installed-p 'sage-shell-mode)
  (package-refresh-contents) (package-install 'sage-shell-mode))
(setq sage-shell:sage-root "~/pkg/SageMath")
;; Run SageMath by M-x run-sage instead of M-x sage-shell:run-sage
(sage-shell:define-alias)
;; Turn on eldoc-mode
(add-hook 'sage-shell-mode-hook #'eldoc-mode)
(add-hook 'sage-shell:sage-mode-hook #'eldoc-mode)
;;;(require 'sage-view "sage-view")
;;;(add-hook 'sage-startup-after-prompt-hook 'sage-view)

;; GAP mode
(use-package gap-mode :defer t)

;; Ebib.
(use-package ebib :defer t)

;; Add zoom for image viewer.
(use-package image+ :defer t)

(use-package smex
  :defer 5
  :commands smex)

;; Octave mode, from http://www.gnu.org/software/octave/doc/interpreter/Using-Octave-Mode.html
;; Note that in the line below, the file name is actually octave-mod, instead of octave-mode.
(autoload 'octave-mode "octave-mod" nil t)
(setq auto-mode-alist (cons '("\\.m$" . octave-mode) auto-mode-alist))
(add-hook 'octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))))

;; Use flyspell.
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
;; Enable flyspell for text mode and disable it for change-log-mode
;; and log-edit-mode.
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))
;; Enable flyspell for nxml mode manually. Seems that nxml-mode does
;; not belong to text-mode?
(add-hook 'nxml-mode-hook 'flyspell-mode)
;; Disable issuing messages when checking buffer.
(setq flyspell-issue-message-flag nil)
;; Spell check spelling in comments.
(add-hook 'lisp-mode-hook 'flyspell-prog-mode)

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
      (quietly-read-abbrev-file)))

;; Htmlize source code.
(use-package htmlize :defer t)

(use-package aria2
  :commands aria2-downloads-list)

(use-package regex-tool
  :commands regex-tool)

;; Load template.
;;;;;(require 'template)

;;; ---------------------- Documentation  ------------------------

(when unix?
  (unless (package-installed-p 'auctex)
    (package-refresh-contents) (package-install 'auctex))
  (load "auctex.el" nil t t)
  ;; It seems that following line is not needed as preview-late can work out of box.
                                        ;(load "preview-latex.el" nil t t)
  ;; Make RefTeX to work with AUCTeX
  (setq reftex-plug-into-AUCTeX t)
  ;; For AUCTex, when hitting C-c C-c, run pdflatex instead of latex.
  (setq TeX-PDF-mode t)
  ;; Make AUCTeX aware of style files and multi-file documents.
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (defun toggle-pdflatex ()
    "Toggle LaTeX processing with PDF or not."
    (interactive)
    (setq TeX-PDF-mode (if TeX-PDF-mode nil t))
    (message "%s" TeX-PDF-mode))
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (outline-minor-mode 1)
              (toggle-pdflatex))))

;;; ------------------ Programming languages ---------------------

;; https://github.com/Malabarba/aggressive-indent-mode
(use-package aggressive-indent
  :hook ((emacs-lisp-mode css-mode) . aggressive-indent-mode))

;; Company-mode. Will add backends for corresponding languages separately.
(use-package company
  :defer 5
  :diminish
  :commands (company-mode company-indent-or-complete-common)
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-dabbrev-downcase nil)
  (setq company-idle-delay 0.5)
  (global-company-mode 1))

;; https://github.com/expez/company-quickhelp
(use-package company-quickhelp
  :defer t
  :after company
  :config
  (add-hook 'company-mode-hook 'company-quickhelp-mode))

(use-package company-math
  :defer t)

(use-package math-symbol-lists
  :defer t)

;; Flycheck.
(use-package flycheck
  :defer t
  :init (global-flycheck-mode)
  (evil-leader/set-key
    "en"       'flycheck-next-error
    "ep"       'flycheck-previous-error))

(require 'init-lsp)

;; Python mode setup with lsp.
;; Based on https://github.com/emacs-lsp/lsp-mode
;; Python side, I'm using Anaconda, so I creates a virtual environment and install
;; related packges.
;;    conda create -n py3.6 python=3.6
;;    conda install -n py3.6 pip
;;    source activate py3.6
;;    pip install --upgrade pip
;;    pip install 'python-language-server[all]'
;; After activating the virtual environment, start Emacs from shell.
;; It's better to also install all Python packages in the virtual environment (e.g.
;; numpy, scipy, matplotlib, pandas, sympy).

(use-package lsp-python
  :after lsp-mode
  ;; Macro lsp-python-enable is created below.
  :hook (python-mode . lsp-python-enable)
  :init
  ;; get lsp-python-enable defined
  ;; NB: use either projectile-project-root or ffip-get-project-root-directory
  ;;     or any other function that can be used to find the root directory of a project
  (lsp-define-stdio-client lsp-python "python"
                           #'projectile-project-root
                           '("pyls")))

;; Python mode.
(use-package python-mode :defer t)

;; ein according to https://github.com/millejoh/emacs-ipython-notebook
(use-package ein :defer t)
(require 'ein)
(require 'ein-loaddefs)
(require 'ein-notebook)
(require 'ein-subpackages)
(setq ein:jupyter-default-server-command "~/anaconda3/envs/py3.6/bin/jupyter"
      ein:jupyter-default-notebook-directory "~/tutorial/pytorch")
;; Start the server with `M-x ein:jupyter-server-start`.
;; Increase image size on HiDPI screen. From https://github.com/syl20bnr/spacemacs/issues/8770
(defun create-image-2x (oldfun file-or-data &optional type data-p &rest props)
  (let ((original (apply oldfun (append (list file-or-data type data-p) props))))
    (if (memq type '(xpm xbm pbm imagemagick)) ;not sure about xbm,pbm,imagemagick
        original
      (let* ((width-height (image-size original t))
             (width (car width-height))
             (height (cdr width-height))
             (width-2x (* 2 width))
             (height-2x (* 2 height))
             (newprops (plist-put props :format type))
             (newprops (plist-put newprops :width width-2x))
             (newprops (plist-put newprops :height height-2x))
             (newargs (append (list file-or-data 'imagemagick data-p) newprops)))
        (apply oldfun newargs)))))
(advice-add 'create-image :around #'create-image-2x)

;; py-autopep8 from https://github.com/paetzke/py-autopep8.el
;; Make sure autopep8 is already installed in python side (should be
;; included in python-language-server[all] required by LSP.
(use-package py-autopep8
  :hook (python-mode . py-autopep8-enable-on-save))

;; Needs to further check.
(setq gud-pdb-command-name "python -m pdb")
(setq pdb-path '/usr/lib/python3.2/pdb.py
      gud-pdb-command-name (symbol-name pdb-path))
(defadvice pdb (before gud-query-cmdline activate)
  "Provide a better default command line when called interactively."
  (interactive
   (list (gud-query-cmdline pdb-path
                            (file-name-nondirectory buffer-file-name)))))

;; Live py mode: https://github.com/donkirkby/live-py-plugin
(use-package live-py-mode :defer t)
;; Open python file, activate live-py-mode with `M-x live-py-mode`.

;; C/C++ with lsp and cquery. Follow https://github.com/cquery-project/cquery/wiki/Emacs
;; First install AUR package cquery-git.
(defun cquery//enable ()
  (condition-case nil
      (lsp-cquery-enable)
    (user-error nil)))

(use-package cquery
  :commands lsp-cquery-enable
  :hook ((c-mode c++-mode) . cquery//enable)
  :init (setq cquery-executable "/bin/cquery"))

;; For better shell mode.
(add-hook 'sh-mode-hook
          (lambda ()
            (ansi-color-for-comint-mode-on)
            (setq tab-width 2
                  indent-tabs-mode nil
                  tab-stop-list '(2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32))))

;; Use eglot for BASH.
;; (use-package eglot
;;   :ensure-system-package
;;   (bash-language-server . "npm i -g bash-language-server")
;;   :config
;;   (add-hook 'sh-mode-hook 'eglot-ensure))

;; Imaxima.
;; (when unix?
;;   (add-to-list 'load-path "/usr/local/Cellar/maxima/5.37.2/share/maxima/5.37.2/emacs")
;;   (autoload 'imaxima "imaxima" "Image support for Maxima." t)
;;   (setq imaxima-tex-program "/Library/TeX/texbin/latex")
;;   (setq imaxima-dvips-program "/Library/TeX/texbin/dvips")
;;   ;; Make Imaxima image larger
;;   (setq imaxima-scale-factor 2.0))

;; Highlight column 80.
;(use-package column-marker)
;(add-hook 'lisp-mode-hook (lambda () (interactive) (column-marker-1 80)))

(require 'init-slime)

;; Enhanced Ruby mode: https://github.com/zenspider/enhanced-ruby-mode
(use-package enh-ruby-mode
  :mode "\\.rb$")

;; Sql.
(require 'sql)

;; Load w3m.
(use-package w3m :defer t)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;; optional keyboard short-cut
(global-set-key "\C-xm" 'browse-url-at-point)

(setq browse-url-browser-function '(("hyperspec" . w3m-browse-url)
                                    ("pylookup" . w3m-browse-url)
                                    ("." . browse-url-generic))
      browse-url-generic-program "chromium")

;;; ---- Start of Web development. ----

;; web-mode according to http://web-mode.org/
(use-package web-mode
  :mode "\\.html?\\'"
  :mode "\\.js\\'"
  :hook (web-mode . company-mode))

;; emmet-mode according to https://github.com/smihica/emmet-mode
(use-package emmet-mode
  :hook (web-mode css-mode))

;; LSP for html from https://github.com/emacs-lsp/lsp-html
;; First run in cli: npm i -g vscode-html-languageserver-bin
(use-package lsp-html
  :hook (web-mode . lsp-html-enable))

;; LSP for css from https://github.com/emacs-lsp/lsp-css
;; First run in cli: npm i -g vscode-css-languageserver-bin
(defun my-css-mode-setup ()
  (when (eq major-mode 'css-mode)
    ;; Only enable in strictly css-mode, not scss-mode (css-mode-hook
    ;; fires for scss-mode because scss-mode is derived from css-mode)
    (lsp-css-enable)))
(use-package lsp-css
  :hook  ((css-mode . my-css-mode-setup)
          (less-mode . lsp-less-enable)
          ((sass-mode scss-mode) . lsp-scss-enable)))

;; Javascript.
(use-package js2-mode
  :mode "\\.js\\'"
  ;; Better imenu
  :hook (js2-mode . js2-imenu-extras-mode))

;; Follow https://github.com/emacs-lsp/lsp-javascript
;; First run in CLI: npm i -g javascript-typescript-langserver
(use-package lsp-javascript-typescript
  :hook ((js2-mode typescript-mode) . lsp-javascript-typescript-enable))

;; Use eslint and babel with flycheck for JS.
;; Based on http://codewinds.com/blog/2015-04-02-emacs-flycheck-eslint-jsx.html,
;; but removing configurations with React.js JSX.
;; In CLI: npm install -g eslint babel-eslint
;; Run `eslint -v` and make sure babel version is 5.x.
;; Edit `~/.eslintrc` for configuraton.
;; Disable jshint since we prefer eslint checking.
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))
;; Disable json-jsonlist checking for json files.
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(json-jsonlist)))
(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-mode 'javascript-eslint 'js2-mode)
;; Use `C-c ! v` to check flycheck status whether eslint is enabled.
;; Install jsonlint by `npm i -g jsonlint`.

;; Skewer mode according to https://github.com/skeeto/skewer-mode
;; To run skewer,
;;    1. M-x run-skewer to attach a browser to Emacs
;;    2. From a js2-mode buffer with skewer-mode minor mode enabled,
;;    send forms for evaluation (in both Emacs and browser).
;;       C-x C-e   evaluate the form at the point.
;;       C-c C-k   load the current buffer
;;       C-c C-z   skewer repl.
(use-package skewer-mode
  :hook ((js2-mode . skewer-mode)
	 (css-mode . skewer-css-mode)
	 (web-mode . skewer-html-mode)))

(use-package restclient
  :mode ("\\.rest\\'" . restclient-mode))

;;; ---- End of Web development. ----

;;; Utility functions
(defun toggle01 (start end)
  "Toggle 0-1 within the region."
  (interactive "r")
  (subst-char-in-region start end ?0 ?X)
  (subst-char-in-region start end ?1 ?Y)
  (subst-char-in-region start end ?X ?1)
  (subst-char-in-region start end ?Y ?0))

(add-to-list 'load-path (expand-file-name "mine" user-emacs-directory))

(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loading %s...done (%.3fs)" load-file-name elapsed))

(add-hook 'after-init-hook
          `(lambda ()
             (let ((elapsed
                    (float-time
                     (time-subtract (current-time) emacs-start-time))))
               (message "Loading %s...done (%.3fs) [after-init]"
                        ,load-file-name elapsed))) t)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;;; init.el ends here
