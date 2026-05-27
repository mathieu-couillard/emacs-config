;; -*- lexical-binding: t; -*-
;;; lsp-setup.el --- Central LSP and Tree-sitter configuration

(use-package treesit
  :ensure nil ; Built-in in Emacs 29+
  :config
  (setq treesit-language-source-alist
        '((python "https://github.com/tree-sitter/tree-sitter-python")
          (typst "https://github.com/uben0/tree-sitter-typst")
          (bash "https://github.com/tree-sitter/tree-sitter-bash")
          (yaml "https://github.com/tree-sitter/tree-sitter-yaml")))
  
  ;; Automatically use the -ts- modes for these languages
  (setq major-mode-remap-alist
        '((python-mode . python-ts-mode)
          (bash-mode . bash-ts-mode)
          (typst-mode . typst-ts-mode))))

(use-package eglot
  :ensure nil
  :bind (:map eglot-mode-map
              ("M-." . xref-find-definitions)
              ("M-," . pop-tag-mark))
  :config
  ;; 1. Cleanly push all your custom servers onto the program list
  (setq eglot-server-programs
        (append '((latex-mode                  . ("texlab"))
                  (typst-ts-mode              . ("tinymist" "lsp"))
                  ((python-mode python-ts-mode) . ("pyright-langserver" "--stdio"))
                  ((python-mode python-ts-mode) . ("ruff" "server")))
                eglot-server-programs))

  ;; This tells the backend to turn on spellchecking for both English and French
  (setq-default eglot-workspace-configuration
                '((:tinymist . (:exportPdf "onSave"
                                :formatterMode "typstfmt"
                                :spellcheck (:language "en-US"
                                             :dictionaries [])))))

  ;; Suppress noisy capability mismatches from flooding your view
  (setq eglot-ignored-server-capabilities '(:workspace/didChangeConfiguration)))
  
(use-package flymake
  :ensure nil
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

(provide 'lsp-setup)
;;; lsp-setup.el ends here
