;;; haskell --- mo lambdas, mo problems

;;; Commentary:
;;; Haskell + Intero for now.  We'll see if ghc-mod is better sometime
;;; but Intero seems to be better in most respects.

;;; Code:

(use-package haskell-mode
  :mode "\\.\\(hs\\|lhs\\|hsc\\|cpphs\\|c2hs\\)\\'"
  :general
  (general-nvmap :keymaps 'haskell-mode-map
                 "gI" 'haskell-navigate-imports)

  (general-nmap :keymaps 'haskell-mode-map
                :prefix ","
                "e" '(:ignore t :which-key "edit")
                "ei" 'haskell-mode-format-imports))

(use-package intero
  :after haskell-mode
  :general
  (general-nmap :keymaps 'haskell-mode-map
                :prefix ","
                "i" '(:ignore t :which-key "intero")
                "ir" 'intero-repl
                "iR" 'intero-restart)
  :config
  (add-hook 'haskell-mode-hook 'intero-mode))

(use-package hindent
  :after haskell-mode
  :general
  (general-nvmap :keymaps 'haskell-mode-map
                 :prefix ","
                 "f" '(:ignore t :which-key "format")
                 "ff" 'hindent-reformat-buffer
                 "fd" 'hindent-reformat-decl)

  :config
  ;; TODO: this isn't always activating. Why?
  (add-hook 'haskell-mode-hook 'hindent-mode)
  (setq hindent-reformat-buffer-on-save t))

(use-package hlint-refactor
  :after haskell-mode
  :general
  (general-nvmap :keymap 'haskell-mode-map
                 :prefix ","
                 "er" 'hlint-refactor-refactor-at-point
                 "eR" 'hlint-refactor-refactor-buffer))

;;; haskell.el ends here
