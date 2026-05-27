;; -*- lexical-binding: t; -*-
;;; org-setup.el --- Personal Knowledge Management

(use-package org
  :ensure t
  :commands (org-capture org-agenda)
  :hook ((org-mode . visual-line-mode)
         (org-mode . variable-pitch-mode)
         (org-mode . org-indent-mode))
  :bind (:map org-mode-map
              ("S-C-<left>" . shrink-window-horizontally)
              ("S-C-<right>" . enlarge-window-horizontally)
              ("S-C-<down>" . shrink-window)
              ("S-C-<up>" . enlarge-window)
              ("C-c C-v" . org-babel-execute-src-block)) ; Integrated binding
  :config
  ;; --- Core Settings ---
  (setq org-directory "~/org"
        org-default-notes-file (concat org-directory "/notes.org")
        org-ellipsis " ▾"
        org-hide-emphasis-markers nil
        org-list-allow-alphabetical t
        org-export-with-toc nil
        org-image-actual-width nil
        org-pretty-entities t
        org-pretty-entities-include-sub-superscripts t)

  ;; --- PhD Task Management ---
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAIT(w)" "|" "DONE(d)" "CANCELLED(c)")))

  ;; --- Babel & Execution (The "Anti-Drift" Consolidation) ---
  (setq org-confirm-babel-evaluate
        (lambda (lang body)
          (not (member lang '("python" "latex" "typst" "emacs-lisp")))))

  ;; ;; Put this stuff in snippets/org/
  ;; (require 'org-tempo)
  ;; (setq org-structure-template-alist
  ;;       '(("sh" . "src sh")
  ;;         ("el" . "src emacs-lisp")
  ;;         ("py" . "src python")
  ;;         ("yaml" . "src yaml")
  ;;         ("json" . "src json")
  ;;         ("la" . "src latex")))

  ;; Load languages all at once
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (latex . t)
     (emacs-lisp . t)))

  ;; --- Physics Rendering ---
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5)))

;; --- Secondary Modules ---

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package ox-latex
  :ensure nil
  :after org
  :config
  (setq org-latex-pdf-process
        '("xelatex -interaction nonstopmode -output-directory %o %f"
          "xelatex -interaction nonstopmode -output-directory %o %f")))

(use-package ox-reveal
  :after org
  :config
  (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js"
        org-reveal-mathjax t))

;; Ensure system path for exports
(setenv "PATH" (concat (getenv "PATH") ":/usr/bin/"))
(add-to-list 'exec-path "/usr/bin")

(provide 'org-setup)
;;; org-setup.el ends here
