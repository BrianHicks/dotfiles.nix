;; basics --- the junk drawer, by another name

;;; Commentary:

;; Very few controversial decisions here, hopefully.  Just stuff that makes Emacs
;; better for me.  Experiments labelled as such.

;;; Code:

;; exec-path-from-shell looks for environment variables set in SHELL and brings
;; them into emacs. This stops having to set things like PATH twice.
(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-variables
    '("PATH" "MANPATH"

      ;; easy access for server processes and tasks
      "AWS_ACCESS_KEY" "AWS_SECRET_KEY"

      ;; sekey
      "SSH_AUTH_SOCK"
      ))

  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package smartparens
  :delight
  :init
  (require 'smartparens-config)
  (smartparens-global-mode 1))

;; which-key helps me remember random keybindings and rediscover things I had
;; lost.
(use-package which-key
  :delight
  :init
  (which-key-mode 1))

;; backup files somewhere outside of where project file watchers will pick them
;; up
;;
;; TODO: now that this is in nix the path below will need to change
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 20
      kept-old-versions 5)

;; lock files are really not necessary for how I use emacs
(setq create-lockfiles nil)

;; highlight the current line
(global-hl-line-mode 1)

;; Wrap at 80 characters instead of 60 by default.
(setq-default fill-column 80)

;; add shackle, with which we will define a bunch of custom rules for popup
;; buffers.
(use-package shackle
  :init
  (shackle-mode 1)

  :config
  (setq shackle-rules '(("\\`*helm.*?\\*\\'" :regexp t :align t :size 0.4))))

;; hide some minor modes
(delight 'auto-revert-mode nil "autorevert")
(delight 'undo-tree-mode nil "undo-tree")

;; _ should be a word character
(modify-syntax-entry ?_ "w")

;; "y" or "n" instead of "yes" or "no"
(defalias 'yes-or-no-p 'y-or-n-p)

;; automatically reload tags
(setq tags-revert-without-query t)

;; I care about whitespace
(setq-default c-basic-indent 4
              tab-width 4
              indent-tabs-mode nil)

(global-whitespace-mode 1)
(setq whitespace-style '(face             ;; enable visualization via faces
                         trailing         ;; show trailing blanks
                         tabs             ;; show tabs
                         empty            ;; empty lines at beginning/end of buffer
                         indentation      ;; "wrong" indentation according to indent-tabs-mode
                         space-after-tab  ;; mixing
                         space-before-tab ;; mixing
                         ))

;; save various histories
(savehist-mode 1)

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(run-at-time nil (* 5 60) 'recentf-save-list)

;; window undo/redo
(winner-mode 1)
(general-nmap :prefix "<SPC>"
              "wu" 'winner-undo
              "wU" 'winner-redo)

;; easy file renaming
; http://emacsredux.com/blog/2013/05/04/rename-file-and-buffer/
(defun rename-file-and-buffer ()
  "Rename the current buffer and the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(general-nvmap :prefix "<SPC>"
               "fR" 'rename-file-and-buffer)

;; bisect this configuration to find bugs
(use-package bug-hunter)

;;; basics.el ends here
