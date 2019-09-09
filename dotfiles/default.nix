{ ... }:

let
  sources = import ../nix/sources.nix;

  pkgs = import sources.nixpkgs { };

  niv = import sources.niv { };

  nixfmt = import sources.nixfmt { };
in {
  imports = [ ./direnv.nix ./emacs.nix ./fzf.nix ./git.nix ./neovim.nix ./ssh.nix ./zsh.nix ];

  home.packages = [
    pkgs.ag
    pkgs.awscli
    pkgs.jq
    pkgs.pv
    pkgs.tree
    pkgs.watch
    pkgs.universal-ctags

    # local packages. I know I could use overlays for these (cf
    # https://github.com/jwoudenberg/dotfiles/commit/12bd31b269b82f0dc661140b8df275ef24f41b81)
    # but I don't want to have to symlink into the overlays directory manually.
    (pkgs.callPackage ../pkgs/lorri.nix { })
    (pkgs.callPackage ../pkgs/smart-gen-tags { })

    # remote packages. Probably could do this better but I don't really want to
    # at the moment.
    nixfmt
    (import (fetchTarball "https://cachix.org/api/v1/install") { }).cachix
    niv.niv
  ];

  programs.man.enable = true;

  programs.home-manager = {
    enable = true;
    path = "$HOME/dotfiles.nix/home-manager";
  };
}
