;;; javascript --- assembly language for the web

;;; Commentary:
;;; nothing to say, really.  It's JavaScript

;;; Code:

(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (setq js2-basic-offset 2))

(use-package prettier-js
  :after js2-mode
  :config
  (add-hook 'js2-mode-hook 'prettier-js-mode))

;;; javascript.el ends here
