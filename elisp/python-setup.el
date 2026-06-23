;; -*- lexical-binding: t; -*-
;;; python-setup.el --- Python Logic & Keys

(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

(use-package python
  :ensure nil
  :defer t
  :config
  ;; 1. Environment & Path 
  (let ((pyenv-path (expand-file-name "~/.pyenv/shims/")))
    (setenv "PATH" (concat (getenv "PATH") ":" pyenv-path))
    (add-to-list 'exec-path pyenv-path))

  ;; 2. REPL Setup
  (setq python-shell-interpreter "python3")
  (setq python-shell-interpreter-args "-m IPython --simple-prompt")

  ;; 3. Workspace Configuration (Ruff + Pyright)
  (setq-default eglot-workspace-configuration
                '((:ruff . (:args ["--line-length=98"]
				  :lint (:enable t)
				  :format (:enable t)))
                  (:pyright . (:analysis (:typeCheckingMode "basic")))))

  ;; Tell Eglot to ignore Pyright's missing formatter so it defaults to Ruff
  (setq-default eglot-ignored-server-capabilities
                '(:documentFormattingProvider
                  :documentRangeFormattingProvider))

  ;; 4. The Virtualenv Manager (Robust Auto-Detection)
  (use-package pyvenv
    :ensure t
    :config
    (setq pyvenv-default-virtual-env-name "venv")
    (pyvenv-mode 1)
    (pyvenv-tracking-mode 1))

  ;; 5. Behavior Hook
  (add-hook 'python-ts-mode-hook 
            (lambda ()
              ;; Automatically find and activate a local 'venv' or '.venv' directory
              (when buffer-file-name
                (let ((target-venv (or (locate-dominating-file buffer-file-name "venv")
                                       (locate-dominating-file buffer-file-name ".venv"))))
                  (when target-venv
                    (pyvenv-activate (expand-file-name "venv" target-venv)))))

              (eglot-ensure)         ; Start LSP (now it will use the correct venv binaries!)
              (flyspell-prog-mode)   ; Spellcheck comments/strings only
              (superword-mode)       ; Treat snake_case_vars as one word
              (hs-minor-mode)        ; Code folding
              (set-fill-column 98)   ; Set wrap margin
              (setq-local compile-command (format "python3 %s" buffer-file-name)))))

;; Force Emacs to map traditional python-mode to the modern tree-sitter mode
(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)))

;; --- Keybindings (Scoped strictly to Python) ---
(general-define-key
 :keymaps 'python-ts-mode-map
 :prefix "C-c"
 "C-c" 'recompile                 ; Run simulation
 "C-z" 'run-python                ; Jump to IPython
 "C-o" 'python-sort-imports        ; Sort imports
 )                    ; Documentation

(provide 'python-setup)
;;; python-setup.el ends here
