{ pkgs, ... }:
let
  extras = [
    ./elm.zsh
    ./find-and-edit.zsh
    ./git.zsh
    ./jetpack.zsh
    ./jump.zsh
    ./kill.zsh
    ./nix.zsh
    ./nixify.sh
    ./root.zsh
    ./rubofix.zsh
    ./scripts.zsh
    ./search.zsh
  ];

  extraInitExtra =
    builtins.foldl' (soFar: new: soFar + "\n" + builtins.readFile new) ""
    extras;
in {
  programs.zsh = {
    enable = true;

    dotDir = ".config/zsh";

    enableAutosuggestions = true;
    enableCompletion = true;

    plugins = [{
      name = "fzf-tab";
      src = pkgs.fzf-tab;
    }];

    initExtra = ''
      EDITOR=kak-session
      alias ks=kak-session

      setopt PROMPT_SUBST

      PROMPT="%B%F{blue}%c%f%b %F{blue}»%f "
      RPROMPT=

      # this is just a work thing, and temporary until we permanently enable a new
      # watcher method
      if test "$(uname -s)" = "Darwin"; then
        SPRING_WATCHER_FSEVENT=1
        export SPRING_WATCHER_FSEVENT
      fi
    '' + extraInitExtra;

    history = {
      save = 10000;
      size = 10000;
      share = true;
    };
  };

  home.packages = [ pkgs.kak-session ];
}
