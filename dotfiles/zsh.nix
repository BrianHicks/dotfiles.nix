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
    ./zsh/root.zsh
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
      EDITOR=kak
      setopt PROMPT_SUBST

      PROMPT="%B%F{blue}%c%f%b %F{blue}»%f "
      RPROMPT=

      eval $(${pkgs.thefuck}/bin/thefuck --alias heck)
    '' + extraInitExtra;

    history = {
      save = 10000;
      size = 10000;
      share = true;
    };
  };
}
