; -*- lexical-binding: t; -*-
;;;;;;;;;;;;;;;;;;;
;; install on computer the first time you run this
;; M-x nerd-icons-install-fonts
;; M-x all-the-icons-install-fonts
;; pip install "python-lsp-server[all]"
;; pip install pylsp-mypy pylsp-rope python-lsp-ruff
;; pip install python-lsp-black
;; Make sure there is a folder call ~/roamNotes/

;; Basic UI Configuration ------------------------------------------------------

;; You will most likely need to adjust this font size for your system!
(defvar runemacs/default-font-size 180)

(setq inhibit-startup-message t)

(cond ((eq system-type 'windows-nt)
       ;; Windows-specific code goes here.     (prefer-coding-system 'utf-8-unix)
       (setq coding-system-for-read 'utf-8-unix)
       (setq coding-system-for-write 'utf-8-unix)))

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-f
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Font Configuration ----------------------------------------------------------

;;(set-face-attribute 'default nil :font "Fira Code Retina" :height runemacs/default-font-size)

;; Set the fixed pitch face
;;(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height 260)

;; Set the variable pitch face
;;(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 295 :weight 'regular)

;; Package Manager Configuration -----------------------------------------------

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))


(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t) ;; set this to t if things stop working


(column-number-mode)
(global-display-line-numbers-mode t)

(use-package command-log-mode)

(use-package auto-package-update
   :config
   (setq auto-package-update-delete-old-versions t
         auto-package-update-interval 4)
   (auto-package-update-maybe))

(global-visual-line-mode t)


;; Ivy Configuration -----------------------------------------------------------


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

;; NOTE: The first time you load your configuration on a new machine, you'll
;; need to run the following command interactively so that mode line icons
;; display correctly:
;;


(use-package nerd-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(load-theme 'deeper-blue t)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; Key Binding Configuration ---------------------------------------------------

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package general
  :config
  (general-create-definer rune/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (rune/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")))


(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-tree)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))



(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))
(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(rune/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))





;; Undo-tree

(use-package undo-tree
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-auto-save-history nil))

;; Projectile Configuration ----------------------------------------------------

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Projects/Code")
    (setq projectile-project-search-path '("~/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; LATEX configuration ---------------------------------------------------------

(use-package auctex
  :defer t)

(add-hook 'LaTeX-mode-hook 
          (lambda()
             (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
             (setq TeX-command-default "XeLaTeX")
             (setq TeX-save-query nil)))

;; Yasnippet configuration ----------------------------------------------------

(use-package yasnippet
  :config
  (yas-global-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Org-roam                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org-roam
  :ensure t 
  :init
  (setq org-roam-v2-ack t)
  :custom
  (setq org-roam-directory "~/roamNotes/")
  (org-roam-completion-everywhere t)

;;   ;; Configure the database backend. SQLite is the default and recommended.
;;   ;; If you're using a different database, configure it here.
;;   ;; (setq org-roam-database-config '((:host "localhost" :user "orgroam" ...)))

;;   ;; OPTIONAL: Configure a custom ID function for new Org-roam files.
;;   ;; The default is a UUID, which is generally good. You might use something
;;   ;; like a timestamp or a more human-readable ID if you prefer.
;;   ;; (setq org-roam-capture-new-node-filename "%<%Y%m%d%H%M%S>-${title}.org")

;;   ;; OPTIONAL: Set up Org-roam UI (if you want the graph visualization)
;;   ;; Requires `org-roam-ui` package.
;;   (use-package org-roam-ui
;;     :ensure t
;;     :after org-roam
;;     :config
;;     (setq org-roam-ui-open-on-start nil) ; Don't open UI automatically
;;     ;; (setq org-roam-ui-sync-theme t)    ; Sync theme with Emacs
;;     ;; (setq org-roam-ui-browser-function 'eww) ; Use eww for browser (or 'browse-url-default-browser)
;;     )

  :config
  (org-roam-setup)

;;   ;; OPTIONAL: Customize capture templates.
;;   ;; This is a powerful feature for quickly creating new notes.
;;   ;; Define your templates here.
;;   (setq org-roam-capture-templates
;;         '(("d" "default" plain "%?"
;;            :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n")
;;            :unnarrowed t)
;;           ("r" "reference" plain "%?"
;;            :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n#+ROAM_TAGS: reference\n")
;;            :unnarrowed t)
;;           ("p" "person" plain "%?"
;;            :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n#+ROAM_TAGS: person\n")
;;            :unnarrowed t)))

;;   ;; OPTIONAL: Configure how links are displayed in Org-roam buffers.
;;   ;; (setq org-roam-link-display-format '((file . "%s") (id . "%s")))

;;   ;; OPTIONAL: Set up Org-roam completion (e.g., with Vertico, Consult, Ivy, Helm).
;;   ;; Example for Consult/Vertico:
;;   (with-eval-after-load 'consult
;;     (setq org-roam-node-display-template (concat "${title:100} " (propertize "${tags:20}" 'face 'org-tag)))
;;     (consult-customize
;;      org-roam-node-read
;;      :preview-function #'org-roam-preview-node
;;      :category 'org-roam-node))

  ;; Global keybindings for common Org-roam commands
  ;; You might want to put these in your general keybinding section.
  :bind (("C-c n f" . org-roam-node-find)     ; Find a node
         ("C-c n i" . org-roam-node-insert)    ; Insert a link to a node
         ("C-c n l" . org-roam-buffer-toggle)  ; Toggle Org-roam buffer (backlinks, etc.)
         ("C-c n c" . org-roam-capture)        ; Capture a new node
         ("C-c n g" . org-roam-graph)          ; Open Org-roam graph (if org-roam-ui is not used)
         ("C-c n t" . org-roam-tag-edit)       ; Edit tags for the current node
         ("C-c n o" . org-roam-open-node)      ; Open the node at point
         ;; org-roam-ui specific (if you are using it)
         ("C-c n u" . org-roam-ui-open)        ; Open Org-roam UI in browser
	 :map org-mode-map
	 ("C-M-i"  . completion-at-point)
         )
)

;; ;; --- Optional: Further Org-roam Integrations ---

;; ;; If you use `org-ql` for advanced queries of your Org files:
;; ;; (use-package org-ql-org-roam
;; ;;   :after (org-ql org-roam)
;; ;;   :ensure t)

;; ;; If you want to use `org-brain` with `org-roam`:
;; ;; (use-package org-brain
;; ;;   :after org-roam
;; ;;   :ensure t
;; ;;   :config
;; ;;   ;; Configure org-brain-directory to point to your org-roam directory
;; ;;   (setq org-brain-directory org-roam-directory)
;; ;;   (org-brain-set-config 'org-roam t))

;; ;; Ensure Org-roam is loaded even if you don't use any of the above
;; ;; This is generally handled by the `:ensure t` in `use-package`.
;; (provide 'init-org-roam) ; If you put this in a separate file, provide it.





;; Make bullets look nice
(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(setq org-list-allow-alphabetical t
      org-export-with-toc nil
      org-image-actual-width nil)

(setq org-hide-emphasis-markers nil)

;; For org mode to override default
;; (define-key org-mode-map (kbd "S-C-<left>") 'shrink-window-horizontally)
;; (define-key org-mode-map (kbd "S-C-<right>") 'enlarge-window-horizontally)
;; (define-key org-mode-map (kbd "S-C-<down>") 'shrink-window)
;; (define-key org-mode-map (kbd "S-C-<up>") 'enlarge-window)

;; Org pretty, show LaTeX
(setq org-pretty-entities 1)

;; org more directory hyperlink open nautilus
;; (setq org-file-apps (cons '(directory . "nautilus file://%s") org-file-apps))


;; babel ----------------------------------------------------------------------

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)))

(setq org-confirm-babel-evaluate nil)


;; tempo --------------- this auto generates code blocks with <sh and hit tab.

(require 'org-tempo)
    (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
    (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
    (add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
    (add-to-list 'org-structure-template-alist '("py" . "src python"))
    (add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
    (add-to-list 'org-structure-template-alist '("json" . "src json"))



;; org ox-latex exporter uses system latex -----------------------------------
(setenv "PATH" (concat (getenv "PATH") ":/usr/bin/"))
(add-to-list 'exec-path "/usr/bin")

;; org-reveal configurations. Make slide show presentations -----------------------------------

(use-package ox-reveal)
(setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
(setq org-reveal-mathjax t)



;; Spell check  ----------------------------------------------------------------

(require 'flyspell)

(setq ispell-list-command "--list") ;; run flyspell with aspell, not ispell
;;(add-hook 'prog-mode-hook 'flyspell-prog-mode)
;;(global-set-key [(control c) (f)] 'flyspell-check-previous-highlighted-word)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)


;; Org-onenote  ----------------------------------------------------------------
(use-package org-onenote
  :ensure org-onenote)
(setq org-onenote-section-map '(("lab notes/anti_helmholtz_cavity" . "0-8295D8A7F568EB8C!380") ("lab notes/tunable_resonator" . "0-8295D8A7F568EB8C!384")))
;; Delete the .emacs/org-onenote-oauth2.plstore file before authentificating


;; Company ---------------------------------------------------------------------

(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-box
  :hook (company-mode . company-box-mode))


;; Flycheck --------------------------------------------------------------------
;; Not supported in Eglot I think
(use-package flycheck
  :init
  (global-flycheck-mode))

;; Python ----------------------------------------------------------------------

;; To manually switch to a new virtual environment
;; (use-package pyvenv
;;   :defer t
  
 
;; This is for Eglot
(setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name "~/.pyenv/shims/")))
(setq exec-path (append exec-path (list (expand-file-name "~/.pyenv/shims/"))))

;; Open python files in tree-sitter mode.
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

(use-package eglot
  :defer t
  :bind (:map eglot-mode-map
	      ("C-c C-d" . eldoc)
              ("C-c C-e" . eglot-rename)
              ("C-c C-o" . python-sort-imports)
              ("C-c C-f" . eglot-format-buffer))
  :hook ((python-ts-mode . eglot-ensure)
         (python-ts-mode . flyspell-prog-mode)
         (python-ts-mode . superword-mode)
         (python-ts-mode . hs-minor-mode)
         (python-ts-mode . (lambda () (set-fill-column 88))))
  :config
  (setq-default eglot-workspace-configuration
                '((:pylsp . (:configurationSources ["flake8"]
                             :plugins (
                                       :pycodestyle (:enabled :json-false)
                                       :mccabe (:enabled :json-false)
                                       :pyflakes (:enabled :json-false)
                                       :flake8 (:enabled :json-false
                                                :maxLineLength 88)
                                       :ruff (:enabled t
                                              :lineLength 88)
                                       :pydocstyle (:enabled t
                                                    :convention "google")
                                       :yapf (:enabled :json-false)
                                       :autopep8 (:enabled :json-false)
                                       :black (:enabled t
                                               :line_length 88
                                               :cache_config t)))))))






(setq python-shell-interpreter "python3.12")
(setq python-shell-interpreter-args "-m IPython")
(setq python-shell-completion-native-enable nil)
(add-hook 'python-ts-mode-hook (lambda ()
             (setq-local compile-command (format "python3.12 %s" buffer-file-name))))







;; Dired -----------------------------------------------------------------


(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))

;;(use-package dired-single)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :config
  ;; Doesn't work as expected!
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("png" . "feh")
                                ("mkv" . "mpv"))))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

;; resizing windows.
(global-set-key (kbd "S-C-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>")  'shrink-window)
(global-set-key (kbd "S-C-<up>")    'enlarge-window)

;; Directional window-selection routines
(use-package windmove
  :ensure nil
  :bind*
  (("M-<left>" . windmove-left)
   ("M-<right>" . windmove-right)
   ("M-<up>" . windmove-up)
   ("M-<down>" . windmove-down)))

;; Quickly switch windows
;; (use-package ace-window
;;   :pretty-hydra
;;   ((:title (pretty-hydra-title "Window Management" 'faicon "nf-fa-th")
;;     :foreign-keys warn :quit-key ("q" "C-g"))
;;    ("Actions"
;;     (("TAB" other-window "switch")
;;      ("x" ace-delete-window "delete")
;;      ("X" ace-delete-other-windows "delete other" :exit t)
;;      ("s" ace-swap-window "swap")
;;      ("a" ace-select-window "select" :exit t)
;;      ("m" toggle-frame-maximized "maximize" :exit t)
;;      ("u" toggle-frame-fullscreen "fullscreen" :exit t))
;;     "Resize"
;;     (("h" shrink-window-horizontally "←")
;;      ("j" enlarge-window "↓")
;;      ("k" shrink-window "↑")
;;      ("l" enlarge-window-horizontally "→")
;;      ("n" balance-windows "balance"))
;;     "Split"
;;     (("r" split-window-right "horizontally")
;;      ("R" split-window-horizontally-instead "horizontally instead")
;;      ("v" split-window-below "vertically")
;;      ("V" split-window-vertically-instead "vertically instead")
;;      ("t" toggle-window-split "toggle"))
;;     "Zoom"
;;     (("+" text-scale-increase "in")
;;      ("=" text-scale-increase "in")
;;      ("-" text-scale-decrease "out")
;;      ("0" (text-scale-increase 0) "reset"))
;;     "Misc"
;;     (("o" set-frame-font "frame font")
;;      ("f" make-frame-command "new frame")
;;      ("d" delete-frame "delete frame")
;;      ("<left>" winner-undo "winner undo")
;;      ("<right>" winner-redo "winner redo"))))
;;   :custom-face
;;   (aw-leading-char-face ((t (:inherit font-lock-keyword-face :foreground unspecified :bold t :height 3.0))))
;;   (aw-minibuffer-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 1.0))))
;;   (aw-mode-line-face ((t (:inherit mode-line-emphasis :bold t))))
;;   :bind (([remap other-window] . ace-window)
;;          ("C-c w" . ace-window-hydra/body))
;;   :hook (emacs-startup . ace-window-display-mode)
;;   :config
;;   (defun toggle-window-split ()
;;     (interactive)
;;     (if (= (count-windows) 2)
;;         (let* ((this-win-buffer (window-buffer))
;;                (next-win-buffer (window-buffer (next-window)))
;;                (this-win-edges (window-edges (selected-window)))
;;                (next-win-edges (window-edges (next-window)))
;;                (this-win-2nd (not (and (<= (car this-win-edges)
;;                                            (car next-win-edges))
;;                                        (<= (cadr this-win-edges)
;;                                            (cadr next-win-edges)))))
;;                (splitter
;;                 (if (= (car this-win-edges)
;;                        (car (window-edges (next-window))))
;;                     'split-window-horizontally
;;                   'split-window-vertically)))
;;           (delete-other-windows)
;;           (let ((first-win (selected-window)))
;;             (funcall splitter)
;;             (if this-win-2nd (other-window 1))
;;             (set-window-buffer (selected-window) this-win-buffer)
;;             (set-window-buffer (next-window) next-win-buffer)
;;             (select-window first-win)
;;             (if this-win-2nd (other-window 1))))
;;       (user-error "`toggle-window-split' only supports two windows")))

;;   ;; Bind hydra to dispatch list
;;   (add-to-list 'aw-dispatch-alist '(?w ace-window-hydra/body) t)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ispell-dictionary nil)
 '(package-selected-packages
   '(ace-flyspell all-the-icons-dired auctex auto-package-update
		  command-log-mode company-box counsel-projectile
		  dap-mode dired-hide-dotfiles dired-open
		  doom-modeline doom-themes ef-themes eglot
		  evil-collection exec-path-from-shell flycheck forge
		  general helpful ivy-rich lsp-ivy lsp-pyright lsp-ui
		  org-bullets org-onenote org-roam org-roam-ui
		  ox-reveal pyenv-mode python-mode pyvenv
		  rainbow-delimiters treemacs treepy undo-tree
		  use-package visual-fill-column which-key yasnippet)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

