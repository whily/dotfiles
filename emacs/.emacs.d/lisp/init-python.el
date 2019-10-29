;;; init-python.el --- Configuration for python packages. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Use LSP for completion etc instead of anaconda mode.

;;; Code:

;; Python mode.
(use-package python-mode
  :mode "\\.py\\'"
  :interpreter "python"
  :config
  (add-to-list 'zeal-at-point-mode-alist '(python-mode . "python")))

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
(add-hook 'python-mode-hook #'lsp)

;; ein according to https://github.com/millejoh/emacs-ipython-notebook
(use-package ein)
(require 'ein)
(require 'ein-notebook)
(require 'ein-subpackages)
(setq ein:jupyter-default-server-command "~/anaconda3/envs/py3.6/bin/jupyter"
      ein:jupyter-default-notebook-directory "~/tutorial/pytorch")
;; Start the server with `M-x ein:jupyter-server-start`.

;; May try emacs-jupyter. For example
;;(use-package jupyter)
;; To start a new kernel on the localhost and connect a REPL client to it `M-x jupyter-run-repl'.

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

(provide 'init-python)
;;; init-python.el ends here
