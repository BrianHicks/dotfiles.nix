{ ... }:

let
  sources = import ../nix/sources.nix;

  pkgs = import sources.nixpkgs { };

  niv = import sources.niv { };

  nixfmt = import sources.nixfmt { };

  lorri = import sources.lorri { };
in {
  imports = [ ./direnv.nix ./fzf.nix ./git.nix ./neovim.nix ./ssh.nix ./zsh.nix ];

  home.packages = [
    pkgs.ag
    pkgs.awscli
    pkgs.jq
    pkgs.pv
    pkgs.tree
    pkgs.watch
    pkgs.universal-ctags

    # local packages
    (pkgs.callPackage ../pkgs/smart-gen-tags { })

    # remote packages. Probably could do this better but I don't really want to
    # at the moment.
    (import (fetchTarball "https://cachix.org/api/v1/install") { }).cachix
    lorri
    niv.niv
    nixfmt
  ];

  programs.man.enable = true;

  programs.home-manager = {
    enable = true;
    path = "$HOME/dotfiles.nix/home-manager";
  };
}
