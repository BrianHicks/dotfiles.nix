{ pkgs, ... }:

let
  extras = [
    ./zsh/elm.zsh
    ./zsh/find-and-edit.zsh
    ./zsh/git.zsh
    ./zsh/jetpack.zsh
    ./zsh/jump.zsh
    ./zsh/kill.zsh
    ./zsh/nix.zsh
    ./zsh/nixify.sh
    ./zsh/root.zsh
    ./zsh/rubofix.zsh
    ./zsh/scripts.zsh
  ];

  extraInitExtra =
    builtins.foldl' (soFar: new: soFar + "\n" + builtins.readFile new) ""
    extras;
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
      EDITOR=kak-session
      alias kak=kak-session

      setopt PROMPT_SUBST

      PROMPT="%B%F{blue}%c%f%b %F{blue}»%f "
      RPROMPT=

      eval $(${pkgs.thefuck}/bin/thefuck --alias heck)

      alias g=lazygit
    '' + extraInitExtra;

    history = {
      save = 10000;
      size = 10000;
      share = true;
    };
  };
}
