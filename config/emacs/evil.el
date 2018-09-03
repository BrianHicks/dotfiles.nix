;;; evil --- emacs is a nice OS, just needs a good text editor.

;;; Commentary:
;;; evil!

;;; Code:

(require 'use-package)

(use-package evil
  :init
  (setq evil-want-C-u-scroll 1)
  (evil-mode 1)

  :general
  (general-nmap :prefix "SPC"
                "b" '(:ignore t :wk "buffers")
                "bn" 'evil-next-buffer
                "bp" 'evil-prev-buffer

                "c" '(:ignore t :which-key "compilation")
                "cc" 'compile
                "cr" 'recompile
                "ck" 'kill-compilation

                "l" '(:ignore t :which-key "clean")
                "lw" 'whitespace-cleanup

                "f" '(:ignore t :which-key "files")
                "fs" 'save-buffer

                "w" '(:ignore t :which-key "windows")
                "wh" 'evil-window-left
                "wj" 'evil-window-down
                "wk" 'evil-window-up
                "wl" 'evil-window-right
                "wv" 'evil-window-vsplit
                "ws" 'evil-window-split)

  :config
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd ":") 'evil-repeat-find-char)
    (define-key evil-motion-state-map (kbd ";") 'evil-ex)))

;; surround text with other text. The keybinding is
;; "ys{motion}{surround}" to surround, "cs{old}{new}" to change, or
;; "ds{old}" to delete. In visual mode, it's "S".
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; toggle comments
;; cmd-/ on a line or block
(use-package evil-commentary
  :delight
  :config (evil-commentary-mode 1))

;; add a bunch more matching pairs like `<div></div>'
;; https://github.com/redguardtoo/evil-matchit
(use-package evil-matchit
  :config (global-evil-matchit-mode 1))

;; swap pairs of regions. `gx' to start, `gX' to cancel.
;; https://github.com/Dewdrops/evil-exchange
(use-package evil-exchange
  :config (evil-exchange-install))

;; start a search from `*' or the visual selection
;; https://github.com/bling/evil-visualstar
(use-package evil-visualstar
  :config (global-evil-visualstar-mode 1))

;; use a key sequence instead of `ESC'
;; https://github.com/syl20bnr/evil-escape
(use-package evil-escape
  :delight ;; kinda cool that it puts the sequence in the modeline by default though.
  :config
  (evil-escape-mode 1)
  (setq-default evil-escape-key-sequence "fd"))

;; bindings for ediff
(use-package evil-ediff
  :after evil)

(provide 'vim)

;;; setup-evil.el ends here
