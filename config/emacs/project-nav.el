;;; project-nav --- project drawer

;;; Commentary:

;;; Code:

; dired
; TODO: how to use use-package here?

(general-nmap "-" 'dired-jump)

(general-nmap :prefix "<SPC>"
              "ff" 'dired-jump-other-window)

(general-nmap :keymaps 'dired-mode-map
              ;; overrides
              ";" 'evil-ex

              ; moving around
              "-" 'spacemin/up-directory)

(general-nmap :keymaps 'dired-mode-map
              :prefix ","
              "e" '(:ignore t :wk "encryption")
              "ed" 'epa-dired-do-decrypt
              "ee" 'epa-dired-do-encrypt
              "es" 'epa-dired-do-sign
              "ev" 'epa-dired-do-verify)

;; TODO: for some mysterious reason, Nix doesn't have a dired+. I'll have to dig
;; in later after the innitial port is done.

;; (use-package dired+
;;   :config
;;   (diredp-toggle-find-file-reuse-dir 1)

;;   (setq diredp-hide-details-initially-flag t
;;         diredp-hide-details-propagate-flag t))

;; from https://github.com/syl20bnr/spacemacs/blob/c7a103a772d808101d7635ec10f292ab9202d9ee/layers/%2Bvim/vinegar/funcs.el#L42-L55
(defun spacemin/up-directory (&optional other-window)
  "Run Dired on parent directory of current directory."
  (interactive "P")
  (let* ((dir (dired-current-directory))
         (orig (current-buffer))
         (up (file-name-directory (directory-file-name dir))))
    (or (dired-goto-file (directory-file-name dir))
        ;; Only try dired-goto-subdir if buffer has more than one dir.
        (and (cdr dired-subdir-alist)
             (dired-goto-subdir up))
        (progn
          (kill-buffer orig)
          (dired up)
          (dired-goto-file dir)))))

;;; project-nav.el ends here
