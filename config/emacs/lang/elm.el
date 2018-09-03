;;; elm --- types and tangrams

;;; Commentary:
;;; it's Elm!

;;; Code:

(use-package elm-mode
  :mode "\\.elm\\'"

  :general
  (general-nvmap :prefix ","
                 :keymaps 'elm-mode-map
                 "c" '(:ignore t :which-key "compile")
                 "cc" 'elm-compile-buffer

                 "e" '(:ignore t :which-key "edit")
                 "ea" 'elm-compile-add-annotations
                 "eI" 'elm-compile-clean-imports
                 "ef" 'elm-mode-format-buffer)

  :config
  (setq elm-tags-on-save t
        elm-format-on-save t)

  (add-to-list 'company-backends 'company-elm))

(use-package flycheck-elm
  :after 'elm-mode
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-elm-setup))

;; TODO: use Juan's newly published package instead of this
;; (use-package elm-test-el
;;   ;; :after 'elm-mode

;;   :straight
;;   (:host github :repo "juanedi/elm-test-el" :branch "master")

;;   :general
;;   (general-nvmap :prefix ","
;;                  :keymaps 'elm-mode-map
;;                  "t" '(:ignore t :which-key "test")
;;                  "tv" 'elm-test-run
;;                  "tt" 'elm-test-rerun
;;                  "tp" 'elm-test-run-project
;;                  "td" 'elm-test-run-directory

;;                  "g" '(:ignore t :which-key "go")
;;                  "gt" 'elm-test-toggle-test-and-target)

;;   :config
;;   (setq elm-test-preferred-test-suffix "Spec"))

(defun spacemin/elm-module-for-path ()
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
