{ pkgs, ... }:

{
  imports = [
    ./config/git.nix
  ];

  programs.man.enable = true;

  programs.ssh = {
    enable = true;

    controlMaster = "auto";
    controlPath = "~/.ssh/control/%r@%h:%p";
    controlPersist = "5m";
  };

  programs.zsh = {
    enable = true;

    dotDir = ".config/zsh";

    enableAutosuggestions = true;

    enableCompletion = true; # TODO: fzf
    # TODO: environment.pathsToLink

    history = {
      # TODO: why do I need both save and size?
      save = 10000;
      size = 10000;
      share = true;
    };
  };

  programs.home-manager = {
    enable = true;
    path = "$HOME/dotfiles.nix/home-manager";
  };
}
