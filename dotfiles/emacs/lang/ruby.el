;;; ruby --- mine some gems

;;; Commentary:
;;; enh-ruby-mode seemed nicer than builtin, but is giving me
;;; issues.  I don't know that it's long for this config.

;;; Code:
(use-package enh-ruby-mode
  :mode ("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode)
  :interpreter ("ruby" . enh-ruby-mode)
  :general
  (general-nvmap :keymaps 'enh-ruby-mode-map
                 :prefix ","
                 "e" '(:ignore t :which-key "edit")
                 "eb" 'enh-ruby-toggle-block
                 "ei" 'enh-ruby-indent-exp

                 "m" '(:ignore t :which-key "meta")
                 "mR" 'erm-reset
                 "mF" 'enh-ruby-fontify-buffer)

  :config
  (setq enh-ruby-add-encoding-comment-on-save nil

        ;; https://github.com/zenspider/enhanced-ruby-mode/issues/36
        enh-ruby-comment-column 32
        enh-ruby-bounce-deep-indent t
        enh-ruby-deep-indent-paren t
        enh-ruby-deep-indent-construct t
        enh-ruby-hanging-paren-deep-indent-level 1
        enh-ruby-hanging-brace-deep-indent-level 1
        enh-ruby-hanging-brace-indent-level 2
        enh-ruby-hanging-indent-level 2
        enh-ruby-hanging-paren-indent-level 2
        enh-ruby-indent-level 2)

  (defun spacemin/setup-ruby-indentation ()
    (setq tab-width 2
          evil-shift-width 2))

  (add-hook 'enh-ruby-mode-hook 'spacemin/setup-ruby-indentation))

(use-package robe
  :after enh-ruby-mode
  :general
  (general-nvmap :keymaps 'enh-ruby-mode-map
                 :prefix ","
                 ;; g for go is already defined
                 "gd" 'robe-jump

                 "h" '(:ignore t :wk "help")
                 "hd" 'robe-doc
                 "hD" 'robe-ask

                 ;; m for meta is already defined
                 "mr" 'robe-start)
  :config
  (add-hook 'enh-ruby-mode-hook 'robe-mode)

  ;; auto completion
  (eval-after-load 'company
    '(push 'company-robe company-backends)))

(use-package rspec-mode
  :after enh-ruby-mode
  :general
  (general-nvmap :keymaps 'enh-ruby-mode-map
                 :prefix ","
                 "t" '(:ignore t :wk "test")
                 "tt" 'rspec-rerun
                 "tv" 'rspec-verify
                 "ts" 'rspec-verify-single
                 "tA" 'rspec-verify-all
                 "tf" 'rspec-run-last-failed
                 "ty" 'rspec-yank-last-command

                 ;; edit already defined
                 "ep" 'rspec-toggle-example-pendingness

                 "g" '(:ignore t :wd "go")
                 "gt" 'rspec-toggle-spec-and-target
                 "gT" 'rspec-find-spec-or-target-other-window)
  ;; TODO: more keybindings!
  :config
  ;; look in spec for lib test files by removing it from 'rspec-primary-source-dirs
  (setq rspec-primary-source-dirs '("app")
        rspec-use-opts-file-when-available nil
        rspec-command-options "--format documentation --profile 30"))

(use-package rbenv
  :init
  (global-rbenv-mode))

(use-package yard-mode
  :after enh-ruby-mode
  :config
  (add-hook 'enh-ruby-mode-hook 'yard-mode))

(use-package rake
  :after enh-ruby-mode
  :general
  (general-nvmap :keymaps 'enh-ruby-mode-map
                 :prefix ","
                 "r" 'rake))

(use-package bundler
  :after enh-ruby-mode
  :general
  (general-nvmap :keymaps 'enh-ruby-mode-map
                 :prefix ","
                 "b" '(:ignore t :wk "bundler")
                 "bs" 'bundle-show
                 "bo" 'bundle-open
                 "bc" 'bundle-check
                 "bI" 'bundle-install
                 "bU" 'bundle-update
                 "be" 'bundle-exec))

(use-package rubocop
  :after enh-ruby-mode
  :general
  (general-nvmap :keymaps 'enh-ruby-mode-map
                 :prefix ","
                 ;; e for edit already defined
                 "ec" 'rubocop-autocorrect-current-file))

;;; ruby.el ends here
