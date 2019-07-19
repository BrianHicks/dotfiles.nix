;;; completion --- gimme suggestions

;;; Commentary:
;;; I'm using company-mode for this now.  This may not be permanent (I
;;; just chose between company and auto-complete in like a minute.)
;;; The point is to get something up and running.

;;; Code:

(use-package company
  :delight
  :config
  (global-company-mode 1)

  (setq company-idle-delay 0.2
        company-selection-wrap-around t)

  (define-key company-active-map [tab] 'company-complete)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

(use-package company-statistics
  :after company
  :init
  ;; TODO: change company-statistics to point somewhere else than ~/.emacs.d?
  (company-statistics-mode))

;;; completion.el ends here
