;;; init-webdev.el --- Configuration for web development.
;;; Commentary:
;;; Configuration for HTML, CSS, and Javascript.

;;; Code:

;; web-mode according to http://web-mode.org/
(use-package web-mode
  :mode "\\.html?\\'"
  :mode "\\.css\\'"
  :mode "\\.js\\'"
  :hook (web-mode . company-mode)
  :bind (:map web-mode-map
              ("C-c C-v" . browse-url-of-buffer))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-enable-current-column-highlight t)
  (web-mode-enable-current-element-highlight t)
  )

;; emmet-mode according to https://github.com/smihica/emmet-mode
(use-package emmet-mode
  :hook (web-mode css-mode))

;; LSP for html from https://github.com/emacs-lsp/lsp-html
;; First run in cli: npm i -g vscode-html-languageserver-bin
(use-package lsp-html
  :hook (web-mode . lsp-html-enable))

;; https://github.com/osv/company-web
(use-package company-web
  :bind (:map web-mode-map
              ("C-'" . company-web-html)))

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

;; Colourise CSS colour literals
(dolist (hook '(css-mode-hook html-mode-hook sass-mode-hook))
  (add-hook hook 'rainbow-mode))

;; Let Emmet aware of embedded CSS
;; TODO: only enable the hook when emmet-mode or web-mode is active,
;; but using hook web-mode-before-auto-complete-hook as from http://web-mode.org/ does not work.
;; On the other hand, post-command-hook might be expensive.
;; Also need to remove the hook.
(add-hook 'post-command-hook
          '(lambda ()
             (when (boundp 'emmet-mode)
               (setq emmet-use-css-transform
                     (string= (web-mode-language-at-pos) "css")))))

;;; TODO: further configure SASS, SCSS, LESS, skewer-less, if needed.

(use-package css-eldoc
  :hook (css-mode . turn-on-css-eldoc))

;; Javascript.
(use-package js2-mode
  :mode "\\.js\\'"
  ;; Better imenu
  :hook (js2-mode . js2-imenu-extras-mode)
  :custom
  (js2-basic-offset 2))

(use-package add-node-modules-path
  :hook (js2-mode . add-node-modules-path))

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

(global-set-key "\C-xm" 'browse-url-at-point)
(add-to-list 'evil-emacs-state-modes 'eww-mode)
(setq browse-url-browser-function '(("hyperspec" . eww-browse-url)
                                    ("pylookup" . eww-browse-url)
                                    ("test" . eww-browse-url)
                                    ("." . browse-url-generic))
      browse-url-generic-program "chromium")

(provide 'init-webdev)
;;; init-webdev.el ends here
