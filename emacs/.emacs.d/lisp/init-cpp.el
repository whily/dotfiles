;;; init-cpp.el --- Configuration for C/C++. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Configuration for C/C++. Use clangd as language server.

;;; Code:

(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

(provide 'init-cpp)
;;; init-cpp.el ends here
