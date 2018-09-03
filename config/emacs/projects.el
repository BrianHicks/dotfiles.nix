;;; projects --- project management

;;; Commentary:

;;; Code:

(use-package projectile
  :general
  (general-nmap :prefix "SPC"
                "p" '(:ignore t :which-key "projectile")
                "p!" 'projectile-run-shell-command-in-root
                "p&" 'projectile-run-async-shell-command-in-root
                "pE" 'projectile-edit-dir-locals
                "pD" 'projectile-dired
                "pF" 'projectile-find-file-in-known-projects
                "pT" 'projectile-test-project
                "pR" 'projectile-regenerate-tags
                "pS" 'projectile-save-project-buffers
                "pt" 'projectile-toggle-between-implementation-and-test
                "pV" 'projectile-browser-dirty-projects
                "pa" 'projectile-find-other-file
                "pb" 'projectile-switch-to-buffer
                "pc" 'projectile-compile-project
                "pd" 'projectile-find-dir
                "pe" 'projectile-recentf
                "pf" 'projectile-find-file
                "pg" 'projectile-find-file-dwim
                "pI" 'projectile-invalidate-cache
                "pj" 'projectile-find-tag
                "pk" 'projectile-kill-buffers
                "pl" 'projectile-find-file-in-directory
                "pm" 'projectile-commander
                "po" 'projectile-multi-occur
                "pp" 'projectile-switch-project
                "pq" 'projectile-switch-open-project
                "pr" 'projectile-replace
                "pu" 'projectile-run-project
                "ps" 'projectile-ag)

  :config
  (projectile-mode 1)

  ;; workaround for https://github.com/bbatsov/projectile/issues/1183
  (setq projectile-mode-line
        '(:eval (format " %s " (projectile-project-name)))))

(use-package helm-projectile
  :init
  (helm-projectile-on)

  :config
  (setq helm-projectile-fuzzy-match t))

(use-package ag
  :general
  (general-nmap :prefix "SPC"
                "s" '(:ignore t :which-key "search")
                "sp" 'ag-project
                "sf" 'ag-files))

;;; projects.el ends here
