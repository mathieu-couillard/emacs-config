;; -*- lexical-binding: t; -*-
;;; python-setup.el --- Python Logic & Keys
(use-package python
  :ensure nil
  :defer t
  :config
  ;; 1. Environment & Path (Existing logic)
  (let ((pyenv-path (expand-file-name "~/.pyenv/shims/")))
    (setenv "PATH" (concat (getenv "PATH") ":" pyenv-path))
    (add-to-list 'exec-path pyenv-path))

  ;; 2. REPL Setup
  (setq python-shell-interpreter "python3")
  (setq python-shell-interpreter-args "-m IPython --simple-prompt")

  ;; 3. Workspace Configuration (Ruff + Pyright)
  ;; This tells the LSP servers exactly how to behave for your PhD project
  (setq-default eglot-workspace-configuration
                '((:ruff . (:args ["--line-length=98"]
                            :lint (:enable t)
                            :format (:enable t)))
                  (:pyright . (:analysis (:typeCheckingMode "basic")))))

  ;; 4. The Virtualenv Manager
  (use-package auto-virtualenv
    :ensure t
    :hook (python-mode-hook . auto-virtualenv-mode)
    :config
    (setq auto-virtualenv-default-env-name "venv"))

  ;; 5. Behavior Hook
  (add-hook 'python-ts-mode-hook 
            (lambda ()
              (eglot-ensure)         ; Start LSP
              (flyspell-prog-mode)   ; Spellcheck comments/strings only
              (superword-mode)       ; Treat snake_case_vars as one word
              (hs-minor-mode)        ; Code folding
              (set-fill-column 98)   ; Set wrap margin
              (setq-local compile-command (format "python3 %s" buffer-file-name)))))

;; --- Keybindings (Scoped strictly to Python) ---
(general-define-key
 :keymaps 'python-ts-mode-map
 :prefix "C-c"
 "C-c" 'recompile                 ; Run simulation
 "C-z" 'run-python                ; Jump to IPython
 "C-o" 'eglot-code-actions        ; Sort imports
 "C-e" 'eglot-rename              ; LSP Rename
 "C-f" 'eglot-format-buffer       ; Ruff Format
 "C-d" 'eldoc)                    ; Documentation

(provide 'python-setup)
;;; python-setup.el ends here
