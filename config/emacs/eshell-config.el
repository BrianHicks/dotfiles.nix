;;; eshell-config --- my eshell configuration

;;; Commentary:

;;; Code:

(require 'eshell)
(require 'em-smart)

(setq eshell-where-to-jump 'begin
      eshell-review-quick-commands nil
      eshell-smart-space-goes-to-end t
      eshell-aliases-file "~/.emacs.d/eshell-aliases")

(general-nmap :prefix "<SPC>"
              "'" 'projectile-run-eshell)

;; rails prints things like ^[[0G, which are not really valid ANSI escape
;; sequences. Let's get rid of 'em.
;;
;; modified from https://emacs.stackexchange.com/questions/18457/stripping-stray-ansi-escape-sequences-from-eshell
(defvar spacemin-ansi-escape-re
  (rx (or ?\233 (and ?\e ?\[))
    (char (?0 . ?9))
    (char (?@ . ?_))))

(defun spacemin-remove-ansi-escapes (beg end)
  (save-excursion
    (goto-char beg)
    (while (re-search-forward spacemin-ansi-escape-re end t)
      (replace-match ""))))

(defun spacemin-eshell-remove-ansi-escapes ()
  (spacemin-remove-ansi-escapes eshell-last-output-start eshell-last-output-end))

(add-hook 'eshell-output-filter-functions 'spacemin-eshell-remove-ansi-escapes t)

(provide 'fake-shell)

;;; eshell-config.el ends here
