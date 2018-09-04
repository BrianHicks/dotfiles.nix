{ pkgs, ... }:

let
  configs = [
    {
      file = ./emacs/init.el;
      pkgs = epkgs: [
    epkgs.general
        epkgs.delight
        epkgs.use-package
      ];
    }
    {
      file = ./emacs/evil.el;
      pkgs = epkgs: [
        epkgs.evil
        epkgs.evil-commentary
        epkgs.evil-ediff
        epkgs.evil-escape
        epkgs.evil-exchange
        epkgs.evil-matchit
        epkgs.evil-surround
        epkgs.evil-visualstar
      ];
    }
    {
      file = ./emacs/basics.el;
      pkgs = epkgs: [
        epkgs.bug-hunter
        epkgs.hydra
        epkgs.shackle
        epkgs.smartparens
        epkgs.which-key
        epkgs.exec-path-from-shell
      ];
    }
    {
      file = ./emacs/compilation.el;
      pkgs = epkgs: [];
    }
    {
      file = ./emacs/completion.el;
      pkgs = epkgs: [
        epkgs.company
        epkgs.company-statistics
      ];
    }
    {
      file = ./emacs/diffing.el;
      pkgs = epkgs: [ epkgs.evil-ediff ];
    }
    {
      file = ./emacs/direnv.el;
      pkgs = epkgs: [ epkgs.direnv ];
    }
    {
      file = ./emacs/display.el;
      pkgs = epkgs: [
        epkgs.doom-themes
        epkgs.hl-todo
        epkgs.solaire-mode
        epkgs.telephone-line
      ];
    }
    {
      file = ./emacs/eshell-config.el;
      pkgs = epkgs: [];
    }
    {
      file = ./emacs/folding.el;
      pkgs = epkgs: [ epkgs.origami ];
    }
    {
      file = ./emacs/helm-config.el;
      pkgs = epkgs: [
        epkgs.helm
        epkgs.helm-ag
      ];
    }
    {
      file = ./emacs/git.el;
      pkgs = epkgs: [
        # for some reason these package aren't requiring git as an input.
        # They're just assuming it'll be laying around on the system. Well,
        # fine, let's provide it.  Fix courtesy of danieldk:
        # https://github.com/danieldk/nix-home/blob/master/cfg/emacs.nix#L11
        (epkgs.magithub.overrideAttrs (attrs: {
          nativeBuildInputs = (attrs.nativeBuildInputs or []) ++ [ pkgs.git ];
        }))

        epkgs.evil-magit
        epkgs.magit
      ];
    }
    {
      file = ./emacs/icons.el;
      pkgs = epkgs: [ epkgs.all-the-icons ];
    }
    {
      file = ./emacs/jumping.el;
      pkgs = epkgs: [
        epkgs.ace-window
        epkgs.avy
      ];
    }
    {
      file = ./emacs/linting.el;
      pkgs = epkgs: [
        epkgs.flycheck
        epkgs.flycheck-status-emoji
        epkgs.flycheck-color-mode-line
      ];
    }
    {
      file = ./emacs/project-nav.el;
      pkgs = epkgs: [];
      # pkgs = epkgs: [ epkgs.diredp ];
    }
    {
      file = ./emacs/projects.el;
      pkgs = epkgs: [
        epkgs.ag
        epkgs.helm-projectile
        epkgs.projectile
      ];
    }
    {
      file = ./emacs/project-nav.el;
      pkgs = epkgs: [ epkgs.yasnippet ];
    }

    ## languages ##
    {
      file = ./emacs/lang/coffeescript.el;
      pkgs = epkgs: [ epkgs.coffee-mode ];
    }
    {
      file = ./emacs/lang/csv.el;
      pkgs = epkgs: [ epkgs.csv-mode ];
    }
    {
      file = ./emacs/lang/dockerfile.el;
      pkgs = epkgs: [ epkgs.dockerfile-mode ];
    }
    {
      file = ./emacs/lang/dockerfile.el;
      pkgs = epkgs: [
        epkgs.alchemist
        epkgs.elixir-mode
      ];
    }
    {
      file = ./emacs/lang/elm.el;
      pkgs = epkgs: [
        epkgs.elm-mode
        epkgs.flycheck-elm
      ];
    }
    {
      file = ./emacs/lang/graphviz.el;
      pkgs = epkgs: [ epkgs.graphviz-dot-mode ];
    }
    {
      file = ./emacs/lang/haml.el;
      pkgs = epkgs: [ epkgs.haml-mode ];
    }
    {
      file = ./emacs/lang/haskell.el;
      pkgs = epkgs: [
        epkgs.haskell-mode
        epkgs.hindent
        epkgs.hlint-refactor
        epkgs.intero
      ];
    }
    {
      file = ./emacs/lang/html.el;
      pkgs = epkgs: [
        epkgs.emmet-mode
        epkgs.web-mode
      ];
    }
    {
      file = ./emacs/lang/haml.el;
      pkgs = epkgs: [ epkgs.idris-mode ];
    }
    {
      file = ./emacs/lang/javascript.el;
      pkgs = epkgs: [
        epkgs.js2-mode
        epkgs.prettier-js
      ];
    }
    {
      file = ./emacs/lang/json.el;
      pkgs = epkgs: [ epkgs.json-mode ];
    }
    {
      file = ./emacs/lang/markdown.el;
      pkgs = epkgs: [
        epkgs.markdown-mode
        epkgs.markdown-toc
      ];
    }
    {
      file = ./emacs/lang/nix.el;
      pkgs = epkgs: [ epkgs.nix-mode ];
    }
    {
      file = ./emacs/lang/ruby.el;
      pkgs = epkgs: [
        epkgs.bundler
        epkgs.enh-ruby-mode
        epkgs.rake
        epkgs.rbenv
        epkgs.robe
        epkgs.rspec-mode
        epkgs.rubocop
        epkgs.yard-mode
      ];
    }
    {
      file = ./emacs/lang/rust.el;
      pkgs = epkgs: [ epkgs.rust-mode ];
    }
    {
      file = ./emacs/lang/terraform.el;
      pkgs = epkgs: [
        epkgs.hcl-mode
        epkgs.terraform-mode
      ];
    }
    {
      file = ./emacs/lang/toml.el;
      pkgs = epkgs: [ epkgs.toml-mode ];
    }
    {
      file = ./emacs/lang/yaml.el;
      pkgs = epkgs: [ epkgs.yaml-mode ];
    }
  ];
in
{
  programs.emacs.enable = true;

  programs.emacs.extraPackages = epkgs: builtins.concatLists (builtins.map (config: config.pkgs epkgs) configs);

  home.file.".emacs.d/init.el" = {
    text = builtins.foldl' (soFar: config: soFar + builtins.readFile config.file + "\n") "" configs;
  };
}
