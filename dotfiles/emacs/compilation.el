;;; compilation --- compilation-mode customizations

;;; Commentary:
;;; compilation mode is part of my muscle memory.  Let's make it nice.

;;; Code:
(require 'ansi-color)

(defun colorize-compilation-buffer ()
  "Parse ANSI escape characters in `compilation-mode` buffers."
  (when (eq major-mode 'compilation-mode)
    (ansi-color-apply-on-region compilation-filter-start (point-max))))

(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(setq compilation-environment '("TERM=xterm-256color"))

;;; compilation.el ends here
