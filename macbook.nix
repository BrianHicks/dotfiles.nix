{ pkgs, ... }:

let
  brianhicks-nur = import (builtins.fetchTarball rec {
    name = "brianhicks-nur";
    url = "https://github.com/BrianHicks/nur-packages/archive/034ea19d72e0efd8d17639ae3258b5c07b9f6f18.tar.gz";
    sha256 = "16l9cy0kan6vl15n9kzff947nfxy8f00i6fgh9qn5y2zrmad2y1n";
  }) {};
in
  {
    imports = [
      ./config/direnv.nix
      ./config/emacs.nix
      ./config/fzf.nix
      ./config/git.nix
      ./config/kakoune.nix
      ./config/neovim.nix
      ./config/ssh.nix
      ./config/tmux.nix
      ./config/zsh.nix
    ];

    home.packages = [
      pkgs.ag
      pkgs.awscli
      pkgs.jq
      pkgs.pv
      pkgs.tree
      pkgs.watch
      brianhicks-nur.elm-language-server

      # local packages. I know I could use overlays for these (cf
      # https://github.com/jwoudenberg/dotfiles/commit/12bd31b269b82f0dc661140b8df275ef24f41b81)
      # but I don't want to have to symlink into the overlays directory manually.
      (pkgs.callPackage ./pkgs/lorri.nix { })
    ];

    programs.man.enable = true;

    programs.home-manager = {
      enable = true;
      path = "$HOME/dotfiles.nix/home-manager";
    };
  }
