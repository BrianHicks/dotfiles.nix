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
}
