;;; haml --- HTML, but weirder

;;; Commentary:
;;; this is gonna be really basic.  I use HAML as little as possible.
;;; At work we pretty much only use it to bootstrap Elm rendering.

;;; Code:

(use-package haml-mode
  :mode "\\.haml\\'"
  :config
  (add-hook 'haml-mode-hook
            (setq indent-tabs-mode nil)))

;;; haml.el ends here
