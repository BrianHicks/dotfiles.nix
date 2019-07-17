;;; language-server.el --- language-specific folding, finding, linting, etc

;;; Commentary:
;; this isn't in language-specific subdirectories since LSP is kind of it's own
;; thing.  I'm going to add language-specific shortcuts in here and maybe reduce
;; my language-specific files over time.  The architecture here makes file
;; ownership hard so I may move it around some.

;;; Code:

(use-package lsp-mode
  :hook ((elm-mode . lsp))
  :commands lsp
  :config
  ;; TODO: figure out how to enable snippets
  (setq lsp-enable-snippet nil
        lsp-enable-indentation t))

;: TODO:
;; (use-package lsp-ui :commands lsp-ui-mode)
;; (use-package company-lsp :commands company-lsp)
;; (use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; xref by default for lenses? somehow turn lenses on by default.

;;; language-server.el ends here
