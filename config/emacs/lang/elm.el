;;; elm --- types and tangrams

;;; Commentary:
;;; it's Elm!

;;; Code:

(use-package elm-mode
  :mode "\\.elm\\'"

  :general
  (general-nvmap :prefix ","
                 :keymaps 'elm-mode-map
                 "l" '(:ignore t :which-key "language-server")
                 "ll" 'lsp-describe-session
                 "lR" 'lsp-restart-workspace

                 "lw" '(:ignore t :which-key "workspace")
                 "lww" 'lsp-workspace-folders-switch
                 "lwa" 'lsp-workspace-folders-add
                 "lwr" 'lsp-workspace-folders-remove

                 "d" '(:ignore t :which-eky "describe")
                 "dp" 'lsp-describe-thing-at-point

                 "e" '(:ignore t :which-key "edit")
                 "ep" 'lsp-execute-code-action
                 "ef" 'elm-format-buffer
                 "er" 'lsp-rename
                 "ei" 'elm-extra/import
                 "eI" 'elm-extra/import-from-file

                 "g" '(:ignore t :which-key "go")
                 "gg" 'lsp-find-definition
                 "gr" 'lsp-find-references
                 "gf" 'helm-lsp-workspace-symbol)
  :config
  (add-hook 'elm-mode-hook #'elm-format-on-save-mode)
  ;; (add-hook 'elm-mode-hook #'lsp-lens-show)

  ;;(add-to-list 'company-backends 'company-elm)
  )

(use-package elm-test-runner
  :after elm-mode
  :general
  (general-nvmap :prefix ","
                 :keymaps 'elm-mode-map
                 "t" '(:ignore t :which-key "test")
                 "tv" 'elm-test-runner-run
                 "td" 'elm-test-runner-run-directory
                 "tt" 'elm-test-runner-rerun
                 "ta" 'elm-test-runner-run-project
                 "tw" 'elm-test-runner-watch

                 "g" '(:ignore t :which-key "go")
                 "gt" 'elm-test-runner-toggle-test-and-target)

  :config
  (setq elm-test-runner-preferred-test-suffix "Spec"))

;;; extra stuff from Juan: https://github.com/juanedi/.spacemacs.d/blob/master/layers/elm-extra/funcs.el

(defun elm-extra/import (&optional input)
  "Prompt for an import statement (INPUT) to add to the current file."
  (interactive)
  (let ((statement (read-string "Import statement: " (concat "import " input))))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "^import " nil t)
          (beginning-of-line)
        (forward-line 1)
        (insert "\n"))
      (insert (concat statement "\n")))
    (elm-sort-imports)))

(defun elm-extra/import-from-file ()
  "Select an elm file interactively and add an import for the corresponding module."
  (interactive)
  (let*
      ((all-files (projectile-current-project-files))
       (elm-files (seq-filter (lambda (f) (s-ends-with-p ".elm" f)) all-files))
       (file-name (projectile-completing-read "Module to import: " elm-files)))
    (when file-name
      (let*
          ((full-file-name (expand-file-name file-name (projectile-project-root)))
           (module-name (with-current-buffer (find-file-noselect full-file-name)
                          (elm--get-module-name))))
        (elm-extra/import module-name)))))

(defun elm-extra/show-and-copy-module-name ()
  "Show and copy the current module's name in the minibuffer."
  (interactive)
  (message (kill-new (elm--get-module-name))))

(defun elm-extra/current-module-name ()
  "Get the module name of the current buffer."
  (let*
      ((raw-components
        (file-name-sans-extension (file-relative-name (buffer-file-name) (elm-test-runner--project-root))))
       (components
        (split-string raw-components "/"))
       (modules
        (remove-if (lambda (c) (string-equal c (downcase c)))
                   components)))
    (string-join modules ".")))

(defun elm-extra/elm-module-for-path ()
  "Figure out the module name for a path.  Useful in snippets!"
  (let* ((raw-components
          (replace-regexp-in-string (projectile-project-root)
                                    ""
                                    (file-name-sans-extension (buffer-file-name))))

         (components
          (split-string raw-components "/"))

         (modules
          (remove-if
           (lambda (component) (string-equal component (downcase component)))
           components))
         )
    (string-join modules ".")))

;;; elm.el ends here
