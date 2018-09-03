;;; icons --- graphics!

;;; Commentary:

;;; Code:

;; all the icons!
(use-package all-the-icons
  :general
  (general-nmap :prefix "SPC"
                "i" '(:ignore t :wk "inserting")
                "ii" 'all-the-icons-insert))

(provide 'icons)

;;; icons.el ends here
