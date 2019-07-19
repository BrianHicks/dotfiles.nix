;;; idris --- proofs!

;;; Commentary:
;;; I'm learning this, and will improve the integration as I go.

;;; Code:

(use-package idris-mode
  :mode "\\.idr\\'"
  :general
  ;; TODO: a bunch of keybindings. I'm replicating those from the
  ;; built-in evil mode here (it requires evil-leader and I don't want
  ;; to install it again just for this.)
  ;;
  ;; there are more keybindings at
  ;; https://github.com/idris-hackers/idris-mode/blob/master/idris-keys.el
  (general-nvmap :keymaps 'idris-mode-map
                 :prefix ","
                 "r" 'idris-load-file
                 "t" 'idris-type-at-point
                 "d" 'idris-add-clause
                 "l" 'idris-make-lemma
                 "c" 'idris-case-split
                 "w" 'idris-make-with-block
                 "m" 'idris-add-missing
                 "p" 'idris-proof-search
                 "h" 'idris-docs-at-point))

;; potential future enhancements: https://github.com/david-christiansen/helm-idris

;;; idris.el ends here
