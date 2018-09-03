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
  ];
in
{
  programs.emacs.enable = true;

  programs.emacs.extraPackages = epkgs: builtins.concatLists (builtins.map (config: config.pkgs epkgs) configs);

  home.file.".emacs.d/init.el" = {
    text = builtins.foldl' (soFar: config: soFar + builtins.readFile config.file + "\n") "" configs;
  };
}
