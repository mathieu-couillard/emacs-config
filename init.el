;; Add elisp folder to load-path
(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))

;; System/Encoding settings (Keep here)
(when (eq system-type 'windows-nt)
  (prefer-coding-system 'utf-8-unix)
  (setq coding-system-for-read 'utf-8-unix)
  (setq coding-system-for-write 'utf-8-unix))

(require 'server)
(unless (server-running-p)
  (server-start))
;; Load your setup modules
(require 'package-setup)
(require 'security-setup)
(require 'ui-setup)
(require 'editing-setup) ;; evil, vundo, flyspell
(require 'navigation-setup)
(require 'completion-setup)
(require 'lsp-setup)
(require 'org-setup)
(require 'writing-setup)
(require 'python-setup)


;; Define where the auto-generated "Customize" settings go
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Load it if it exists, but don't complain if it doesn't
(when (file-exists-p custom-file)
  (load custom-file))

(defun mc/first-run-setup ()
  "Install all fonts and external dependencies for a new setup."
  (interactive)
  (nerd-icons-install-fonts t)
  (all-the-icons-install-fonts t)
  (async-shell-command "pip install \"python-lsp-server[all]\" python-lsp-ruff ruff")
  (make-directory "~/roamNotes/" t)
  (message "First-run setup complete! Please install the fonts from the folders that opened."))

