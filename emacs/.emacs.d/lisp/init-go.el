;;; init-go.el --- Configuration for Go. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Configuration for Go.

;;; Code:

;; https://github.com/dominikh/go-mode.el

(use-package go-mode
  :mode "\\.go\\'")

(use-package go-playground)

;; Install gopls throught socks5 proxy:
;;     git config --global http.proxy socks5://127.0.0.1:1080
;;     https_proxy=socks5://127.0.0.1:1080 go get golang.org/x/tools/gopls@latest
;;     git config --global --unset http.proxy


(provide 'init-go)
;;; init-go.el ends here
