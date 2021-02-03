{ ... }:

let
  sources = import ../nix/sources.nix;
  pkgs = import sources.nixpkgs { };
  niv = import sources.niv { };
in {
  imports = [
    ./bat.nix
    ./direnv.nix
    ./fzf.nix
    ./git.nix
    ./kakoune.nix
    ./ssh.nix
    ./tmux.nix
    ./zsh.nix
  ];

  home.packages = [
    pkgs.ag
    pkgs.hyperfine
    pkgs.jq
    pkgs.ripgrep
    pkgs.tree
    pkgs.watch
    (pkgs.callPackage ../pkgs/percollate { })
    (pkgs.callPackage sources.comma { })

    # remote packages. Probably could do this better but I don't really want to
    # at the moment.
    (import (fetchTarball "https://cachix.org/api/v1/install") { }).cachix
    niv.niv
    pkgs.nixfmt
  ];

  programs.man.enable = true;
  programs.home-manager.enable = true;
}
