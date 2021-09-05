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
    # remote packages. Probably could do this better but I don't really want to
    # at the moment.
    pkgs.cachix
    niv.niv
    pkgs.nixfmt
  ];

  programs.man.enable = true;
  programs.home-manager.enable = true;

  manual.html.enable = true; # adds home-manager-help
  manual.manpages.enable = true;
}
