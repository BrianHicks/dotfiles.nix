;;; markdown --- kinda nice, really.

;;; Commentary:
;;; not a lot to see here, move along.

;;; Code:

(use-package markdown-mode
  :mode "\\.md\\'")

(use-package markdown-toc
  :general
  (general-nvmap :keymaps 'markdown-mode-map
                 :prefix ","
                 "e" '(:ignore t :wk "edit")
                 "et" 'markdown-toc-generate-or-refresh-toc))

(provide 'markdown)

;;; markdown.el ends here
