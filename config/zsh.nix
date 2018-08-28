{ pkgs, ... }:

{
  programs.zsh = {
    enable = true;

    dotDir = ".config/zsh";

    enableAutosuggestions = true;

    enableCompletion = true;
    # TODO: environment.pathsToLink (see home-configuration.nix man page)

    history = {
      # TODO: why do I need both save and size?
      save = 10000;
      size = 10000;
      share = true;
    };
  };
}
