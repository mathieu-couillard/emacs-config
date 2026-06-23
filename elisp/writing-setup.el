;; -*- lexical-binding: t; -*-
;;; writing-setup.el --- Academic Writing (LaTeX + PDF)

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
	 (LaTeX-mode . flyspell-mode)
         (LaTeX-mode . visual-line-mode)
         (LaTeX-mode . TeX-source-correlate-mode) ;; live update in pdf-tools
         (LaTeX-mode . prettify-symbols-mode); Restores the "pretty symbols" look
         ;; 2. Robust UI initialization (Highlighting & Folding)
         (LaTeX-mode . (lambda () ;; this ensure the pretty math symbols stay pretty
                         (require 'tex-fold)
                         (require 'font-latex)
                         (TeX-fold-mode 1)
                         (font-latex-setup)
                         (font-lock-flush)
                         (font-lock-ensure)
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
  :ensure t
  :vc (:url "https://codeberg.org/meow_king/typst-ts-mode" 
	    :rev :head)
  :mode "\\.typ\\'"
  :hook ((typst-ts-mode . eglot-ensure)
         (typst-ts-mode . flymake-mode) 
         (typst-ts-mode . typst-ts-watch-mode) 
         ;; (typst-ts-mode . visual-line-mode)
         (typst-ts-mode . (lambda ()
                            (setq word-wrap t)
                            (setq truncate-lines nil)))
         ;; NEW FIX: Silently revert the PDF inside Emacs without crashing layouts
         (typst-ts-mode . (lambda ()
                            (add-hook 'compilation-finish-functions
                                      (lambda (buf str)
                                        (let ((pdf-buf (get-buffer (replace-regexp-in-string "\\.typ" ".pdf" (buffer-name)))))
                                          (when (buffer-live-p pdf-buf)
                                            (with-current-buffer pdf-buf
                                              (pdf-view-revert-buffer nil t)))))
                                      nil t))))
  :bind (:map typst-ts-mode-map
              ("C-c C-c" . typst-ts-compile)
              ("C-c C-w" . typst-ts-watch-mode)) 
  :custom
  (typst-ts-mode-watch-options '("--open"))
  
  :config
  (unless (fboundp 'typst-ts-watch-mode)
    (defalias 'typst-ts-watch-mode #'typst-ts-mode-watch-toggle))

  ;; Force window rules to completely hide the brief 0.2s process popups
  (add-to-list 'display-buffer-alist
               '("\\*typst-ts-compilation\\*"
                 (display-buffer-no-window)))) ; No window prevents layout shifts entirely

(provide 'writing-setup)
;;; writing-setup.el ends here
