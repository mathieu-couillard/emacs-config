;;; ui-setup.el --- Personal UI configuration  -*- lexical-binding: t; -*-

(defvar runemacs/default-font-size 120
  "Default font size for the Emacs frame.")

;; IBM Plex Mono, Input narrow Mono
(defvar runemacs/fixed-pitch-font "IBM Plex Mono"
  "The font used for code and monospaced text.")

;; Lato, Rufina, Josefin Sans
(defvar runemacs/variable-pitch-font "Lato"
  "The font used for prose and writing (variable-pitch).")

;; Set the font (Actually apply the variable above)
(set-face-attribute 'default nil :height runemacs/default-font-size)

;; The main font for the editor
(set-face-attribute 'default nil 
                    :font runemacs/fixed-pitch-font 
                    :height runemacs/default-font-size)

;; For code blocks and data tables (always stays monospaced)
(set-face-attribute 'fixed-pitch nil 
                    :font runemacs/fixed-pitch-font 
                    :height 1.0)

;; For "Writing" mode (prose, notes)
(set-face-attribute 'variable-pitch nil 
                    :font runemacs/variable-pitch-font 
                    :height 1.1
		    :width 'normal)

;; Ensure syntax highlighting stays monospaced in variable-pitch-mode
(custom-theme-set-faces
 'user
 `(font-lock-comment-face ((t (:inherit fixed-pitch :italic t))))
 `(font-lock-keyword-face ((t (:inherit fixed-pitch :bold t))))
 `(font-lock-variable-name-face ((t (:inherit fixed-pitch))))
 `(font-lock-function-name-face ((t (:inherit fixed-pitch)))))


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
