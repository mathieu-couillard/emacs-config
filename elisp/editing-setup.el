;;; editing-setup.el --- navigation infrastructure -*- lexical-binding: t; -*-

;; Vim keybindings
(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo)

  ;; use instead of escape
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  ;; use instead of backspace
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; hjkl navigation in Insert Mode
  (define-key evil-insert-state-map (kbd "M-h") 'backward-char)
  (define-key evil-insert-state-map (kbd "M-j") 'evil-next-visual-line)
  (define-key evil-insert-state-map (kbd "M-k") 'evil-previous-visual-line)
  (define-key evil-insert-state-map (kbd "M-l") 'forward-char)

  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;; evil for Dired, Magit, Eshell
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; visual undo
(use-package vundo
  :ensure t
  :bind ("C-x u" . vundo)
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

;; save undo history in file somewhere
(use-package undohist
  :ensure t
  :config
  (undohist-initialize))

;; Spell-checker
(use-package flyspell
  :ensure nil ; Built-in to Emacs
  :hook ((text-mode . flyspell-mode)       ; Spell check for prose (Org, LaTeX, Markdown)
         (prog-mode . flyspell-prog-mode) ; Spell check only comments/strings in code
	 (typst-ts-mode . flyspell-mode)) 
  :bind (:map flyspell-mode-map
         ("C-;" . flyspell-correct-wrapper))
  :config
  ;; Use aspell for better performance and dictionary support
  (setq ispell-program-name "aspell"
        ispell-list-command "--list"
        ispell-extra-args '("--sug-mode=ultra")) ; Adjust lang if needed
  ;; Optional: If you find the "ping" sound annoying on a misspelled word
  (setq visible-bell t))

(use-package guess-language
  :ensure t
  :hook (text-mode . guess-language-mode)
  :config
  (setq guess-language-languages '(en fr)
        guess-language-langcodes '((en "en_US" "English")
                                   (fr "fr_FR" "French"))
        guess-language-min-paragraph-length 30)
  
  ;; Trigger flyspell update immediately upon language detection
  (add-hook 'guess-language-after-detection-functions
            (lambda (lang _begin _end)
              (ispell-change-dictionary (car (assoc lang guess-language-langcodes))))))

;; Version control
(use-package magit
  :bind ("C-x g" . magit-status)   ; The "Magic" shortcut to open the Git status
  :config
  ;; Fixes the "free variable" warning by setting it only after Magit loads
  (setq magit-auto-revert-mode t)
  ;; This ensures the status buffer opens in a full window or a logical spot.
  (setq magit-display-buffer-function 
        #'magit-display-buffer-same-window-except-diff-v1))


(provide 'editing-setup)
;;; editing-setup.el ends here
