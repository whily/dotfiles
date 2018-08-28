;;; init.el --- My emacs configuration file.
;;; Commentary:
;;; With reference to configuration from purcell and spacemacs.

;;; Code:

;; For debug purpose. When encoutering an EmacsLisp error, this will
;; pop up a BacktraceBuffer.
(setq debug-on-error t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;;; ----------------------- General ----------------------------

;; As a fan of Common Lisp.
(require 'cl)

(require 'init-elpa)

;; EVIL.
(use-package evil)
(use-package evil-leader)
(global-evil-leader-mode)
(evil-mode 1)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "<SPC>"    'execute-extended-command
  "hdb"      'describe-bindings
  "hdf"      'describe-function
  "hdv"      'describe-variable)

;; https://github.com/seagle0128/doom-modeline
;; Run M-x all-the-icons-install-fonts to install fonts included with all-the-icons.
(use-package doom-modeline
  :ensure t
  :defer t
  :hook (after-init . doom-modeline-init))

(use-package dashboard
  :ensure
  :config
  (dashboard-setup-startup-hook))

(use-package rainbow-mode)

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

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

;; Ivy, swiper, and counsel: http://oremacs.com/swiper/
(use-package counsel
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "))

;; avy https://github.com/abo-abo/avy
;; Maybe try https://github.com/tam17aki/ace-isearch ?
(use-package avy
  :config
  (global-set-key (kbd "C-c j") 'avy-goto-word-or-subword-1))

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
(use-package unicode-fonts)
(unicode-fonts-setup)

;; Set font.
(set-frame-font "Inconsolata 18" nil t)

;; Highlight selected region.
(setq transient-mark-mode t)

;; Disable Electric Indent mode.
(electric-indent-mode 0)

;; Show battery status.
(setq battery-mode-line-format " [%b%p%%]")
(display-battery-mode)

;; Setup Ediff to use one frame only, which is useful for Stumpwm.
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;; ------------------- Development tools -----------------------

;; Projectile from https://github.com/bbatsov/projectile
;; To manually mark a project folder (which is not version controlled),
;; create file named .projectile
(use-package projectile
  :demand
  :init   (setq projectile-use-git-grep t)
  :config
  (projectile-global-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; Zeal at point: https://github.com/jinzhu/zeal-at-point
(use-package zeal-at-point
  :bind (("\C-cd" . zeal-at-point))
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
  :config
  (yas-global-mode 1))
(use-package yasnippet-snippets)

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
(use-package gap-mode)

;; Ebib.
(use-package ebib)

;; Add zoom for image viewer.
(use-package image+)

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
(setq-default abbrev-mode t)
(setq abbrev-file-name "~/.emacs.d/.abbrev_defs"
      save-abbrevs t)

;; Htmlize source code.
(use-package htmlize)

;; Regex tool.
;;;;;(load "regex-tool" t)

;; Load template.
;;;;;(require 'template)

;;; ---------------------- Documentation  ------------------------

(when unix?
  (unless (package-installed-p 'auctex)
    (package-refresh-contents) (package-install 'auctex))
  ; Add MacTex path.
  (setenv "PATH" (concat "/Library/TeX/texbin:" (getenv "PATH")))
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

;; Company-mode. Will add backends for corresponding languages separately.
(use-package company
  :ensure t
  :init (global-company-mode)
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-dabbrev-downcase nil)
  (setq company-idle-delay 0.5))

;; https://github.com/expez/company-quickhelp
(use-package company-quickhelp
  :config
  (add-hook 'company-mode-hook 'company-quickhelp-mode))

;; Flycheck.
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

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
(use-package lsp-mode
  :ensure t
  :config

  ;; make sure we have lsp-imenu everywhere we have LSP
  (require 'lsp-imenu)
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
  ;; get lsp-python-enable defined
  ;; NB: use either projectile-project-root or ffip-get-project-root-directory
  ;;     or any other function that can be used to find the root directory of a project
  (lsp-define-stdio-client lsp-python "python"
                           #'projectile-project-root
                           '("pyls"))

  ;; make sure this is activated when python-mode is activated
  ;; lsp-python-enable is created by macro above
  (add-hook 'python-mode-hook
            (lambda ()
              (lsp-python-enable)))

  ;; lsp extras
  (use-package lsp-ui
    :ensure t
    :config
    (setq lsp-ui-sideline-ignore-duplicate t)
    (add-hook 'lsp-mode-hook 'lsp-ui-mode))

  (use-package company-lsp
    :config
    (setq company-lsp-enable-snippet t)
    (push 'company-lsp company-backends)))

;; Python mode.
(use-package python-mode)

;; ein according to https://github.com/millejoh/emacs-ipython-notebook
(use-package ein)
(require 'ein)
(require 'ein-loaddefs)
(require 'ein-notebook)
(require 'ein-subpackages)
(setq ein:jupyter-default-server-command "~/anaconda3/envs/py3.6/bin/jupyter"
      ein:jupyter-default-notebook-directory "~/tutorial/pytorch")
;; Start the server with `M-x ein:jupyter-server-start`.
:
;; py-autopep8 from https://github.com/paetzke/py-autopep8.el
;; Make sure autopep8 is already installed in python side (should be
;; included in python-language-server[all] required by LSP.
(use-package py-autopep8
  :config
  (add-hook 'python-mode-hook 'py-autopep8-enable-on-save))

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
(use-package live-py-mode)
;; Open python file, activate live-py-mode with `M-x live-py-mode`.

;; C/C++ with lsp and cquery. Follow https://github.com/cquery-project/cquery/wiki/Emacs
;; First install AUR package cquery-git.
(defun cquery//enable ()
  (condition-case nil
      (lsp-cquery-enable)
    (user-error nil)))

(use-package cquery
  :commands lsp-cquery-enable
  :init (setq cquery-executable "/bin/cquery"))
(add-hook 'c-mode-hook #'cquery//enable)
(add-hook 'c++-mode-hook #'cquery//enable)

;; For better shell mode.
(add-hook 'sh-mode-hook
          (lambda ()
            (ansi-color-for-comint-mode-on)
            (setq tab-width 2
                  indent-tabs-mode nil
                  tab-stop-list '(2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32))))

;; LSP for shell
;; First install Bash Language Server from https://github.com/mads-hartmann/bash-language-server
;; Below commands does not work, check later.
;;    sudo npm i -g bash-language-server
;; No melpa package yet: https://github.com/emacs-lsp/lsp-sh

;; Imaxima.
(when unix?
  (add-to-list 'load-path "/usr/local/Cellar/maxima/5.37.2/share/maxima/5.37.2/emacs")
  (autoload 'imaxima "imaxima" "Image support for Maxima." t)
  (setq imaxima-tex-program "/Library/TeX/texbin/latex")
  (setq imaxima-dvips-program "/Library/TeX/texbin/dvips")
  ;; Make Imaxima image larger
  (setq imaxima-scale-factor 2.0))

;; Highlight column 80.
(use-package column-marker)
(add-hook 'lisp-mode-hook (lambda () (interactive) (column-marker-1 80)))

;; Setup SLIME for SBCL.
;; Prepareing SLIME.
(use-package slime
   :if unix?
   :init
   (setq inferior-lisp-program "sbcl")
   (setq slime-contribs '(slime-asdf slime-fancy)))

  ;; (require 'slime-autoloads)
  ;; (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
  ;; (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
  ;; (add-to-list 'load-path (concat-dir "slime/contrib"))
  ;; (setq slime-complete-symbol*-fancy t
  ;;       slime-complete-symbol-function 'slime-fuzzy-complete-symbol
  ;;       common-lisp-hyperspec-root "file:/usr/share/doc/hyperspec/HyperSpec/"
  ;;       slime-net-coding-system 'utf-8-unix)
  ;; (add-hook 'slime-mode-hook
  ;;           (lambda ()
  ;;             (setq slime-truncate-lines nil)
  ;;             (slime-redirect-inferior-output)))
  ;; ;; == Normal SLIME stuff.
  ;; (global-set-key "\C-cs" 'slime-selector)
  ;; (global-set-key "\M-i" 'slime-fuzzy-complete-symbol)
  ;; ;; From http://bc.tech.coop/blog/081209.html
  ;; (global-set-key "\C-c;" 'slime-insert-balanced-comments)
  ;; (global-set-key "\C-c\M-;" 'slime-remove-balanced-comments))

;; Cider for clojure.
;; Install first.
;(use-package cider)
;; Configuration according to https://github.com/clojure-emacs/cider
;; Enable eldoc in Clojure buffer
;(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
;; Hide the *nrepl-connection* and *nrepl-server* buffers from
;; appearing in some buffer switching commands
;(setq nrepl-hide-special-buffers t)
;; Prevent the auto-display of the REPL buffer in a separate window
;; after connection is established.
;(setq cider-repl-pop-to-buffer-on-connect nil)
;; Stop the error buffer from popping up while working in buffers
;; other than the REPL.
;(setq cider-popup-stacktraces nil)
;; Enable error buffer popping also in the REPL.
;(setq cider-repl-popup-stacktraces t)
;; To auto-select the error buffer when it's displayed.
;(setq cider-auto-select-error-buffer t)

;; Enhanced Ruby mode: https://github.com/zenspider/enhanced-ruby-mode
(use-package enh-ruby-mode
  :mode "\\.rb$")

;; Sql.
(require 'sql)

;; Load w3m.
(use-package w3m)
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
  :ensure t
  :mode "\\.html?\\'"
  :mode "\\.js\\'"
  :config
  (add-hook 'web-mode-hook 'company-mode))

;; emmet-mode according to https://github.com/smihica/emmet-mode
(use-package emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode) ; Enable emmet whenever web-mode is on.
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

;; LSP for html from https://github.com/emacs-lsp/lsp-html
;; First run in cli: sudo npm i -g vscode-html-languageserver-bin
(use-package lsp-html
  :config
  (add-hook 'web-mode-hook #'lsp-html-enable))

;; LSP for css from https://github.com/emacs-lsp/lsp-css
;; First run in cli: sudo npm i -g vscode-css-languageserver-bin
(defun my-css-mode-setup ()
  (when (eq major-mode 'css-mode)
    ;; Only enable in strictly css-mode, not scss-mode (css-mode-hook
    ;; fires for scss-mode because scss-mode is derived from css-mode)
    (lsp-css-enable)))
(use-package lsp-css
  :config
  (add-hook 'css-mode-hook #'my-css-mode-setup)
  (add-hook 'less-mode-hook #'lsp-less-enable)
  (add-hook 'sass-mode-hook #'lsp-scss-enable)
  (add-hook 'scss-mode-hook #'lsp-scss-enable))

;; Javascript.
(use-package js2-mode
  :mode "\\.js\\'"
  :config
  ;; Better imenu
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode))
;; Follow https://github.com/emacs-lsp/lsp-javascript
;; First run in CLI: sudo npm i -g javascript-typescript-langserver
(use-package lsp-javascript-typescript
  :config
  (add-hook 'js2-mode-hook #'lsp-javascript-typescript-enable)
  (add-hook 'typescript-mode-hook #'lsp-javascript-typescript-enable)
  (add-hook 'js3-mode-hook #'lsp-javascript-typescript-enable)
  (add-hook 'rjsx-mode #'lsp-javascript-typescript-enable))

;; Use eslint and babel with flycheck for JS.
;; Based on http://codewinds.com/blog/2015-04-02-emacs-flycheck-eslint-jsx.html,
;; but removing configurations with React.js JSX.
;; In CLI: sudo npm install -g eslint babel-eslint
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

;; Skewer mode according to https://github.com/skeeto/skewer-mode
;; To run skewer,
;;    1. M-x run-skewer to attach a browser to Emacs
;;    2. From a js2-mode buffer with skewer-mode minor mode enabled,
;;    send forms for evaluation (in both Emacs and browser).
;;       C-x C-e   evaluate the form at the point.
;;       C-c C-k   load the current buffer
;;       C-c C-z   skewer repl.
(use-package skewer-mode
  :config
  (add-hook 'js2-mode-hook 'skewer-mode)
  (add-hook 'css-mode-hook 'skewer-css-mode)
  (add-hook 'web-mode-hook 'skewer-html-mode))

;;; ---- End of Web development. ----

;;; Utility functions
(defun toggle01 (start end)
  "Toggle 0-1 within the region."
  (interactive "r")
  (subst-char-in-region start end ?0 ?X)
  (subst-char-in-region start end ?1 ?Y)
  (subst-char-in-region start end ?X ?1)
  (subst-char-in-region start end ?Y ?0))

(add-to-list 'load-path "~/.emacs.d/mine")

;;; Customizations.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (dracula-theme avy live-py-mode yasnippet-snippets w3m htmlize enh-ruby-mode web-mode auto-package-update keychain-environment material-theme better-defaults zeal-at-point use-package unicode-fonts slime projectile nasm-mode markdown-mode magit js2-mode image+ gap-mode ebib column-marker cider auctex)))
 '(py-shell-name "python3")
 '(python-shell-interpreter "~/anaconda3/bin/python3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 89)) (:foreground "#ffffff" :background "#263238")))))
