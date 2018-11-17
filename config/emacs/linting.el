;;; linting -- add flycheck checking

;;; Commentary:
;;; it's flycheck!

;;; Code:

(use-package flycheck
  :defer 1
  :init
  (global-flycheck-mode)

  :general
  (general-nmap :prefix "SPC"
                "e" '(:ignore t :which-key "errors")
                "el" 'flycheck-list-errors
                "en" 'flycheck-next-error
                "ep" 'flycheck-previous-error))

(use-package flycheck-status-emoji
  :after flycheck
  :init (flycheck-status-emoji-mode 1))

(use-package flycheck-color-mode-line
  :after flycheck
  :init (flycheck-color-mode-line-mode 1))

;;; linting.el ends here
