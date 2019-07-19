;;; terraform.el --- edit .tf and .hcl files

;;; Commentary:

;;; Code:

(use-package hcl-mode
  :mode "\\.hcl\\'")

(use-package terraform-mode
  :mode "\\.tf\\'"
  :config
  (add-hook 'terraform-mode-hook 'terraform-format-on-save-mode))

;;; terraform.el ends here
