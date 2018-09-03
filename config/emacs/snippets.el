;;; snippets.el --- commonly typed things

;;; Commentary:

;;; Code:

; TODO: port my snippets!

(use-package yasnippet
  :general
  (general-imap "<backtab>" 'yas-expand)

  :config
  (yas-global-mode 1))

;;; snippets.el ends here
