{ pkgs, ... }:

let
  sources = import ../nix/sources.nix;

  niv = import sources.niv { };
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
    (import (fetchTarball "https://github.com/serokell/nixfmt/archive/e4f31f45799554ff378370256a24f606a3025b0a.tar.gz")
    { })
    (import (fetchTarball "https://cachix.org/api/v1/install") { }).cachix
    niv.niv
  ];

  programs.man.enable = true;

  programs.home-manager = {
    enable = true;
    path = "$HOME/dotfiles.nix/home-manager";
  };
}
