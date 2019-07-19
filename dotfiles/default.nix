{ pkgs, ... }:

{
  imports = [
    ./direnv.nix
    ./emacs.nix
    ./fzf.nix
    ./git.nix
    ./neovim.nix
    ./ssh.nix
    ./zsh.nix
  ];

  home.packages = [
    pkgs.ag
    pkgs.awscli
    pkgs.jq
    pkgs.pv
    pkgs.tree
    pkgs.watch

    # local packages. I know I could use overlays for these (cf
    # https://github.com/jwoudenberg/dotfiles/commit/12bd31b269b82f0dc661140b8df275ef24f41b81)
    # but I don't want to have to symlink into the overlays directory manually.
    (pkgs.callPackage ../pkgs/lorri.nix { })
  ];

  programs.man.enable = true;

  programs.home-manager = {
    enable = true;
    path = "$HOME/dotfiles.nix/home-manager";
  };
}
