;;; ui-setup.el --- Personal UI configuration  -*- lexical-binding: t; -*-

(defvar runemacs/default-font-size 100
  "Default font size for the Emacs frame.")

;; Set the font (Actually apply the variable above)
(set-face-attribute 'default nil :height runemacs/default-font-size)

;; Visual Feedback & Spacing
(set-fringe-mode 10)        ; Give the text some breathing room
(setq visible-bell t)       ; Flash screen instead of beeping
(column-number-mode)        ; Show column in modeline
(setq-default line-move-visual t)

;; Line Numbers
(require 'display-line-numbers)
(global-display-line-numbers-mode t)

;; Disable line numbers for specific modes
(dolist (mode '(term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode -1))))

;; Icons - essential for doom-modeline
(use-package nerd-icons
  :if (display-graphic-p))

(use-package command-log-mode)

;; Modeline setup
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; Visual aids for coding
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Load theme last to ensure it overrides package defaults
(load-theme 'deeper-blue t)

(provide 'ui-setup)
;;; ui-setup.el ends here
