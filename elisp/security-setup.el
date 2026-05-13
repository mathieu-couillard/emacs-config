;;; security-setup.el --- Auth and secrets -*- lexical-binding: t; -*-

(use-package auth-source
  :ensure t
  :config
  (setq auth-sources '("~/.authinfo.gpg" "~/.authinfo"))
  (setq auth-source-cache-expiry 28800)
  
  (defun my-auth-source-test ()
    "Test auth-source debugging function..."
    (interactive)
    (let ((auth-info (auth-source-search :host "quantum-memory2.local"
                                         :user "hqdteam"
					 :port 22)))
      (if auth-info
          ;; auth-source-search returns a list of plists.
          ;; (car auth-info) gets the first plist.
          ;; plist-get extracts the value for a given key (e.g., :user, :host, :secret).
          ;; The secret is a function that needs to be called to get the actual password.
          (message "Found password for %s@%s: %s"
                   (plist-get (car auth-info) :user)
                   (plist-get (car auth-info) :host)
                   (funcall (plist-get (car auth-info) :secret)))
        (message "No password found for quantum-memory2.local"))))
  )

(provide 'security-setup)
;;; security-setup.el ends here
