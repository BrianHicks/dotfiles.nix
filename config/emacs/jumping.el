;;; jumping --- move around the screen efficiently

;;; Commentary:
;;; This will probably end up being a bunch of keys across packages
;;; all in one place.  It should probably be towards the end of the
;;; load list.

;;; Code:

(use-package avy
  :general
  (general-nvmap :prefix "SPC"
                 "j" '(:ignore t :wk "jumping")
                 "jd" 'avy-goto-word-1))

(use-package ace-window
  :general
  (general-nmap :prefix "SPC"
                "jw" 'ace-window))

;;; jumping.el ends here
