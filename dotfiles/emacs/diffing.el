;;; diffing.el --- nice diff resolving in emacs

;;; Commentary:

;;; Code:

(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally)

  ;; restore window layout after ediff quits
  ;; from the emacs wiki (https://www.emacswiki.org/emacs/EdiffMode)
  (defvar spacemin-ediff-last-windows nil)

  (defun spacemin-store-pre-ediff-winconfig ()
    (setq spacemin-ediff-last-windows (current-window-configuration)))

  (defun spacemin-restore-pre-ediff-winconfig ()
    (set-window-configuration spacemin-ediff-last-windows))

  (add-hook 'ediff-before-setup-hook #'spacemin-store-pre-ediff-winconfig)
  (add-hook 'ediff-quit-hook #'spacemin-restore-pre-ediff-winconfig)
  )

(use-package evil-ediff
  :after ediff)

;;; diffing.el ends here
