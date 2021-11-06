;;; init-go.el --- Configuration for Go. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Configuration for Go.

;;; Code:

;; https://github.com/dominikh/go-mode.el

(use-package go-mode
  :mode "\\.go\\'")

;; Install gopls: FAILED!
;;   mkdir -p $GOPATH/src/golang.org/x
;;   cd !$
;;   git clone https://github.com/golang/tools
;;   go install golang.org/x/tools/gopls

(provide 'init-go)
;;; init-go.el ends here
