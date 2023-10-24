{ pkgs, ... }:
let
  extras = [
    ./elm.zsh
    ./find-and-edit.zsh
    ./flixify.sh
    ./git.zsh
    ./jetpack.zsh
    ./jump.zsh
    ./llvm-13.zsh
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
in
{
  programs.zsh = {
    enable = true;

    dotDir = ".config/zsh";

    enableAutosuggestions = true;
    enableCompletion = true;

    plugins = [
      {
        name = "fzf-tab";
        src = pkgs.fzf-tab;
      }
    ];

    initExtra =
      ''
        EDITOR=nvim
        export EDITOR

        # see `man zshoptions`
        setopt PROMPT_SUBST # do parameter, command, and arithmetic expansion in prompts
        setopt PROMPT_SP    # preserve partial lines, adding inverse-colored % after a line

        EMOJI_NEUTRAL=(😃 🤓 👾 🤖 💯 🐵 🦍 🐺 🐈 🦄 🦅 🦉 🐬 🐋 🐙 🌲 🌳 🍀 🍁 🍇 🍍 🍩 🌍 🌎 🌏 🚄 🚍 🚲 🛴 🚡 🚠 🚀)
        EMOJI_UNHAPPY=(😵 😲 🤡 👹 😿 💔 💢)
        EMOJI_PROMPT="''${EMOJI_NEUTRAL[$RANDOM % ''${#EMOJI_NEUTRAL[@]}]}"
        EMOJI_PROMPT_ERROR="''${EMOJI_UNHAPPY[$RANDOM % ''${#EMOJI_UNHAPPY[@]}]}"

        PROMPT="%(?.$EMOJI_PROMPT .$EMOJI_PROMPT_ERROR [%F{red}%?%f] )%B%F{blue}%c%f%b %F{blue}»%f "
        RPROMPT=

        # 1password plugins
        alias gh="op plugin run -- gh"

        # grab Homebrew binaries if needed
        if test -d /opt/homebrew/bin; then
          export PATH="$PATH:/opt/homebrew/bin"
        fi

        # make delete key work
        bindkey "^[[3~" delete-char
      ''
      + extraInitExtra;

    history = {
      save = 10000;
      size = 10000;
      share = true;
    };
  };

  home.packages = [ pkgs.kak-session pkgs.mand ];
}
