;;; git --- vcs!

;;; Commentary:

;;; Code:

(use-package magit
  :general
  (general-nmap :prefix "SPC"
                "g" '(:ignore t :which-key "git")
                "gs" 'magit-status
                "gb" 'magit-blame)

  :config
  (setq git-commit-fill-column 72))

(use-package evil-magit
  :after magit)

;; TODO:
;;
;; - shortcut to jump to a Jenkins build for a branch

;;; git.el ends here
