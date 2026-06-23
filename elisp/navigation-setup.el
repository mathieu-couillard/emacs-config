;; -*- lexical-binding: t; -*-
;;; navigation-setup.el --- navigation infrastructure


;; =============================================================================
;; 1. Minibuffer Completion (Ivy, Counsel, Helpful)
;; =============================================================================

;; Add info to minibuffer
(use-package ivy
  :after evil
  :diminish
  :bind (("C-s" . swiper)
	 ("C-M-s" . swiper-thing-at-point)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))


;; adds docstring to M-x minibuffer, extra columns to C-x b, and 
(use-package ivy-rich
  :after (ivy counsel)
  :ensure t
  :init
  (setq ivy-rich-path-style 'abbreviated)
  :config
  (ivy-rich-mode 1))

;; Search interface in minibuffer, filter as you write
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . counsel-minibuffer-history)))

;; open cheat-sheet with typing prefixes like C-x, C-c... 
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

;; Adds stuff to the default Emacs Help, uses Counsel to display it.
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; =============================================================================
;; 2. Project Management (Projectile)
;; =============================================================================

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (setq projectile-cache-file (expand-file-name "cache/projectile.cache" user-emacs-directory))
  (setq projectile-known-projects-file (expand-file-name "cache/projectile-bookmarks.eld" user-emacs-directory))
  (setq projectile-switch-project-action #'projectile-dired))

;; 2. Load the "Bridge" (Counsel + Projectile)
(use-package counsel-projectile
  :config (counsel-projectile-mode))


;; =============================================================================
;; 3. File Management (Dired)
;; =============================================================================

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (with-eval-after-load 'evil-collection
    (evil-collection-define-key 'normal 'dired-mode-map
      "h" 'dired-up-directory
      "l" 'dired-find-alternate-file)))



(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :after dired
  :config
  (setq dired-open-extensions '(("pdf" . "evince"))))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (with-eval-after-load 'evil-collection
    (evil-collection-define-key 'normal 'dired-mode-map
      "H" 'dired-hide-dotfiles-mode)))

;; =============================================================================
;; 4. Interface Enhancements (Hydra & General)
;; =============================================================================

;; Make tree like structure for keybindings, 
(use-package hydra)
(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

; Keybinding manager 
(use-package general
  :config
  (general-create-definer rune/leader-keys
    :keymaps '(normal visual emacs)
    :prefix "SPC" ; when in normal mode
    :global-prefix "C-SPC") ; when in insert mode
  (rune/leader-keys
    "p"  '(:ignore t :which-key "projects")
    "pp" '(projectile-switch-project :which-key "switch project")
    "pf" '(projectile-find-file :which-key "find file in project")
    "pa" '(projectile-add-known-project :which-key "add project")
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")
    "ts" '(hydra-text-scale/body :which-key "scale text")
    "s"  '(:ignore t :which-key "search")
    "ss" '(swiper :which-key "buffer")
    "sp" '(counsel-projectile-rg :which-key "project")   ;; Search everything in the project
    "sf" '(counsel-find-file :which-key "find file")
    "sb" '(counsel-switch-buffer :which-key "switch buffer")))

;; =============================================================================
;; 5. Window Management (Navigation & Resizing)
;; =============================================================================

;; Resizing windows with Ctrl+Shift+Arrows
(global-set-key (kbd "S-C-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>")  'shrink-window)
(global-set-key (kbd "S-C-<up>")    'enlarge-window)

;; Directional window-selection (Alt+Arrows)
(use-package windmove
  :ensure nil
  :bind
  (("M-<left>" . windmove-left)
   ("M-<right>" . windmove-right)
   ("M-<up>" . windmove-up)
   ("M-<down>" . windmove-down)
   ("M-C-<left>" . windmove-swap-states-left)
   ("M-C-<right>" . windmove-swap-states-right)
   ("M-C-<up>" . windmove-swap-states-up)
   ("M-C-<down>" . windmove-swap-states-down)))

(provide 'navigation-setup)
;;; navigation-setup.el ends here
