;;; helm-config --- fuzzy find eveyrthing!

;;; Commentary:

;;; Code:

(use-package helm
  :delight
  :general
  (general-nvmap :prefix "SPC"
                 "<SPC>" 'helm-M-x

                 "bb" 'helm-mini

                 ;; jumping prefix defined in jumping.el
                 "jj" 'helm-semantic-or-imenu

                 "y" '(:ignore t :wk "yanking")
                 "yp" 'helm-show-kill-ring)

  (general-nmap "-" 'helm-find-files)
  :config
  (helm-mode 1)

  (setq helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-display-function 'pop-to-buffer
        helm-file-cache-fuzzy-match t
        helm-recentf-fuzzy-match t
        helm-semantic-fuzzy-match t
        helm-window-prefer-horizontal-split t))


(use-package helm-ag
  :after helm)

;;; helm-config.el ends here
