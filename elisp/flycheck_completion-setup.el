;;; completion-setup.el --- Corfu + Yasnippet -*- lexical-binding: t; -*-

;; Auto-complete front end. Drop down menu
(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)                 ; Enable auto-completion
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 2)
  (corfu-cycle t)                ; Allow looping through suggestions
  :init
  (global-corfu-mode))

(use-package yasnippet
  :ensure t
  :hook (after-init . yas-global-mode)
  :config
  (setq yas-snippet-dirs 
        (list (expand-file-name "snippets" user-emacs-directory))) 
  (yas-reload-all))

;; This package allows Corfu to see your snippets
(use-package yasnippet-capf
  :ensure t
  :after (corfu yasnippet)
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

;; Visual icons for Corfu
(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(provide 'completion-setup)
;;; completion-setup.el ends here
