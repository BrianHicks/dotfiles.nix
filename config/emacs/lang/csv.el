;;; csv.el --- comma separated text files

;;; Commentary:
;;; I don't open these in Emacs very much, but when I do I really really want this.

;;; Code:

(use-package csv-mode
  :mode "\\.\\(c\\|t\\)sv\\'"
  :general
  (general-nvmap :prefix ","
                 :keymaps 'csv-mode-map
                 "t" 'csv-transpose
                 "k" 'csv-kill-fields
                 "s" 'csv-sort-fields
                 "S" 'csv-reverse-region
                 "f" 'csv-align-fields
                 "F" 'csv-unalign-fields))

;;; csv.el ends here
