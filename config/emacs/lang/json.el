;;; json --- a little better than XML.

;;; Commentary:
;;; nothing to say, really.  It's JSON.

;;; Code:

(use-package json-mode
  :mode "\\.json\\'"
  :general
  (general-nvmap :keymaps 'json-mode-map
                 :prefix ","
                 "e" '(:ignore t :which-key "edit")
                 "ef" 'json-mode-beautify
                 "et" 'json-toggle-boolean
                 "eD" 'json-nullify-sexp

                 "y" '(:ignore t :which-key "yanking")
                 "yp" 'json-mode-kill-path)
  )

;;; json.el ends here
