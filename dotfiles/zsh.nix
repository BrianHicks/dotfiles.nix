{ pkgs, ... }:

let
  extras = [
    ./zsh/elm.zsh
    ./zsh/git.zsh
    ./zsh/jetpack.zsh
    ./zsh/jump.zsh
    ./zsh/kill-process.zsh
    ./zsh/neovim.zsh
    ./zsh/nix.zsh
    ./zsh/nixify.sh
    ./zsh/rubofix.zsh
    ./zsh/scripts.zsh
  ];

  extraInitExtra = builtins.foldl' (soFar: new: soFar + "\n" + builtins.readFile new) "" extras;
in {
  home.packages = [
    # for shell autocorrections
    pkgs.thefuck
  ];

  programs.zsh = {
    enable = true;

    dotDir = ".config/zsh";

    enableAutosuggestions = true;
    enableCompletion = true;

    initExtra = ''
      EDITOR=vim

      eval $(${pkgs.thefuck}/bin/thefuck --alias heck)
    '' + extraInitExtra;

    history = {
      save = 10000;
      size = 10000;
      share = true;
    };
  };
}
