{ ... }:

let
  sources = import ../nix/sources.nix;

  pkgs = import sources.nixpkgs { };

  niv = import sources.niv { };

  nixfmt = import sources.nixfmt { };

  brianhicks-nur = import sources.brianhicks-nur { };
in {
  imports =
    [ ./bat.nix ./direnv.nix ./fzf.nix ./git.nix ./neovim.nix ./ssh.nix ./starship.nix ./taskwarrior.nix ./zsh.nix ];

  home.packages = [
    pkgs.ag
    pkgs.jq
    pkgs.tree
    pkgs.watch

    # remote packages. Probably could do this better but I don't really want to
    # at the moment.
    (import (fetchTarball "https://cachix.org/api/v1/install") { }).cachix
    niv.niv
    nixfmt
  ];

  programs.man.enable = true;

  programs.home-manager = {
    enable = true;
    path = "$HOME/dotfiles.nix/home-manager";
  };
}
