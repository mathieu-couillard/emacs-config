;; -*- lexical-binding: t; -*-
;;; lsp-setup.el --- Central LSP and Tree-sitter configuration

(use-package treesit
  :ensure nil ; Built-in in Emacs 29+
  :config
  (setq treesit-language-source-alist
        '((python "https://github.com/tree-sitter/tree-sitter-python" "v0.23.6")
          (typst "https://github.com/uben0/tree-sitter-typst")
          (bash "https://github.com/tree-sitter/tree-sitter-bash")
          (yaml "https://github.com/tree-sitter/tree-sitter-yaml")))
  
  ;; Automatically use the -ts- modes for these languages
  (setq major-mode-remap-alist
        '((python-mode . python-ts-mode)
          (bash-mode . bash-ts-mode)
          (typst-mode . typst-ts-mode))))


(use-package eglot
  :ensure t
  :defer t
  :bind (:map eglot-mode-map
              ("C-c C-d" . eldoc)
              ("C-c C-e" . eglot-rename))
  :config
  ;; Cap connection timeout at 5 seconds so a failing server never deadlocks your UI
  (setq eglot-connect-timeout 5)
  
  ;; Suppress noisy capability mismatches from flooding your view
  (setq eglot-ignored-server-capabilities 
        '(:workspace/didChangeConfiguration :documentFormattingProvider))

  ;; Global workspace configurations for non-python tools
  (setq-default eglot-workspace-configuration
                '((tinymist . (:exportPdf "onSave"
					  :formatterMode "typstfmt"
					  :spellcheck (:language "en-US,fr"
								 :dictionaries []))))))

(use-package apheleia
  :ensure t
  :init
  (apheleia-global-mode +1)
  :config
  ;; --- Python Configuration ---
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) 'ruff)

  ;; --- Typst Configuration ---
  ;; 1. Define how to call the typstyle CLI
  (setf (alist-get 'typstyle apheleia-formatters) '("typstyle"))
  ;; 2. Map the major mode to the formatter definition
  (setf (alist-get 'typst-ts-mode apheleia-mode-alist) 'typstyle))

(use-package flymake
  :ensure nil
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

(provide 'lsp-setup)
;;; lsp-setup.el ends here
