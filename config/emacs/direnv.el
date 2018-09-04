;;; direnv -- use direnv

;;; Commentary:

;;; Code:

(use-package direnv-mode
  :general
  (general-nmap :prefix "SPC"
    "d" '(:ignore t :wk "direnv")
    "dd" 'direnv-update-environment
    "dD" 'direnv-update-directory-environment
    "de" 'direnv-edit)

  :config
  (direnv-mode)

  (setq direnv-always-show-summary t
        direnv-use-faces-in-summary t))

;;; direnv.el ends here
