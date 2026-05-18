;;; writing-setup.el --- Academic Writing (LaTeX + PDF) -*- lexical-binding: t; -*-

;; PDF viewer
(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (add-hook 'pdf-view-mode-hook (lambda () (cursor-set-step-mode -1))))
;; --------------------------------
;; Latex
;; --------------------------------
;; TeX environment
(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-PDF-mode t)
  (TeX-view-program-selection '((output-pdf "PDF Tools")))
  (TeX-source-correlate-start-server t)
  (TeX-fold-auto-update t) ; Ensures new things you type get folded

  :hook ((LaTeX-mode . eglot-ensure)
         (LaTeX-mode . visual-line-mode)
         (LaTeX-mode . TeX-source-correlate-mode) ;; live update in pdf-tools
         (LaTeX-mode . variable-pitch-mode)
         (LaTeX-mode . prettify-symbols-mode) ; Restores the "pretty symbols" look
         ;; 2. Robust UI initialization (Highlighting & Folding)
         (LaTeX-mode . (lambda ()
                         (require 'tex-fold)
                         (require 'font-latex)
                         (TeX-fold-mode 1)
                         ;; Initialize AUCTeX font engine before painting
                         (font-latex-setup)
                         ;; Refresh colors
                         (font-lock-flush)
                         (font-lock-ensure)
                         ;; Fold only after font-lock knows what the symbols are
                         (TeX-fold-buffer)))
         ;; 3. Automatic PDF buffer refresh after compilation
         (LaTeX-mode . (lambda () 
                         (add-hook 'TeX-after-compilation-finished-functions
                                   #'TeX-revert-document-buffer nil t))))
  
  :config
  ;; 1. Prevent Eglot from stripping colors (Semantic Tokens)
  (setq-default eglot-ignored-server-capabilities '(:semanticTokensProvider))

  ;; 2. Your TexLab workspace configuration
  (with-eval-after-load 'eglot
    (setq-default eglot-workspace-configuration
                  (append eglot-workspace-configuration
                          '((:texlab . (:build (:executable "latexmk"
                                                :args ["-pdf" "-interaction=nonstopmode" "-synctex=1" "%f"]
                                                :onSave t
                                                :forwardSearchAfter t)
                                        :forwardSearch (:executable "emacsclient"
                                                        :args ["--no-wait" "+%l" "%f"])
                                        :chktex (:onOpen t :onEdit t))))))))
;; --------------------------------
;; Typst
;; --------------------------------

(use-package typst-ts-mode
  :vc (:url "https://codeberg.org/meow_king/typst-ts-mode" 
            :rev :head) ; <-- Fixes the (wrong-type-argument stringp nil) error
  :mode "\\.typ\\'"
  :hook ((typst-ts-mode . eglot-ensure)
         (typst-ts-mode . variable-pitch-mode)
	 (typst-ts-mode . visual-line-mode)
	 (typst-ts-mode . (lambda()
			    (setq word-wrap t)
			    (setq truncate-lines nil))))
  :bind (:map typst-ts-mode-map
              ("C-c C-c" . typst-ts-compile)
              ("C-c C-w" . typst-ts-watch-mode)) ;; Toggle save to also compile
  :custom
  (typst-ts-most-watch-option "--open" ))

(provide 'writing-setup)
;;; writing-setup.el ends here
