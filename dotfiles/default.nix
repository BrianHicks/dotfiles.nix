{ ... }:

let
  sources = import ../nix/sources.nix;

  pkgs = import sources.nixpkgs { };

  niv = import sources.niv { };

  brianhicks-nur = import sources.brianhicks-nur { };
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
    pkgs.du-dust
    pkgs.hyperfine
    pkgs.jq
    pkgs.pastel
    pkgs.ripgrep
    pkgs.tree
    pkgs.watch
    pkgs.zenith
    (import ../pkgs/percollate { inherit pkgs; })

    # remote packages. Probably could do this better but I don't really want to
    # at the moment.
    (import (fetchTarball "https://cachix.org/api/v1/install") { }).cachix
    niv.niv
    pkgs.nixfmt
  ];

  home.file.".config/kitty/kitty.conf".source = ./kitty.conf;

  programs.man.enable = true;
  programs.home-manager.enable = true;
}
