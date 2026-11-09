;;; yaml-setup.el --- YAML configuration -*- lexical-binding: t; -*-

;; Configure YAML support with Tree-sitter and Eglot
(use-package yaml-mode
  :ensure t
  :mode (("\\.yml\\'" . yaml-ts-mode)
         ("\\.yaml\\'" . yaml-ts-mode))
  :hook (yaml-ts-mode . eglot-ensure)
  :config
  ;; Ensure standard 2-space indentation for YAML files
  (setq yaml-indent-offset 2))

(provide 'yaml-setup)
;;; editing-setup.el ends here
