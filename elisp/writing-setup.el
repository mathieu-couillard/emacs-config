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
  :config
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-PDF-mode t
        TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-mode t
        TeX-source-correlate-start-server t)

  :hook ((LaTeX-mode . eglot-ensure) ; Start LSP when opening LaTeX
         (LaTeX-mode . visual-line-mode)
         (LaTeX-mode . TeX-source-correlate-mode)
         (LaTeX-mode . TeX-fold-mode)
         (LaTeX-mode . (lambda () 
                         (add-hook 'TeX-after-compilation-finished-functions
                                   #'TeX-revert-document-buffer)))))

;; --- Texlab / Eglot Integration ---
  (with-eval-after-load 'eglot
    (setq-default eglot-workspace-configuration
                  (append eglot-workspace-configuration
                          '((:texlab . (:build (:executable "latexmk"
                                                :args ["-pdf" "-interaction=nonstopmode" "-synctex=1" "%f"]
                                                :onSave t
                                                :forwardSearchAfter t)
                                        :forwardSearch (:executable "emacsclient"
                                                        :args ["--no-wait" "+%l" "%f"])
                                        :chktex (:onOpen t :onEdit t)))))))


;; --------------------------------
;; Typst
;; --------------------------------

(use-package typst-ts-mode
  :vc (:url "https://codeberg.org/meow_king/typst-ts-mode" 
            :rev :head) ; <-- Fixes the (wrong-type-argument stringp nil) error
  :mode "\\.typ\\'"
  :hook (typst-ts-mode . eglot-ensure)
  :bind (:map typst-ts-mode-map
              ("C-c C-c" . typst-ts-compile)
              ("C-c C-w" . typst-ts-watch-mode)) ;; Toggle save to also compile
  :custom
  (typst-ts-most-watch-option "--open" ))

(provide 'writing-setup)
;;; writing-setup.el ends here
