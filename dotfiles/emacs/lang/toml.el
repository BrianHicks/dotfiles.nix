;;; toml --- like INI, but not INI

;;; Commentary:
;;; Rust uses this, so it's here.  I don't much care for it.

;;; Code:

(use-package toml-mode
  :mode "/\\(.+\\.toml\\|Cargo.lock\\|.cargo/config\\)\\'")

;;; toml.el ends here
