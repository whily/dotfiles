;;; init-llm.el --- Configuration for Large Language Model. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Configuration for LLM.

;;; Code:
;; https://github.com/s-kostyaev/ellama

(use-package ellama
  :init
  ;; setup key bindings
  (setopt ellama-keymap-prefix "C-c e")
  ;; language you want ellama to translate to
  (setopt ellama-language "English")
  (require 'llm-ollama)
  (setopt ellama-provider
	  (make-llm-ollama
	   ;; This model should be pulled to use it
	   ;; value should be the same as you print in terminal during pull
	   :chat-model "mistral"
	   :embedding-model "mistral")))

(provide 'init-llm)
;;; init-llm.el ends here
