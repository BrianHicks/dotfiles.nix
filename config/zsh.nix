{ pkgs, ... }:

let
  extras = [
    ./zsh/elm.zsh
    ./zsh/jetpack.zsh
    ./zsh/jump.zsh
    ./zsh/kill-process.zsh
    ./zsh/nixify.sh
    ./zsh/rubofix.zsh
    ./zsh/scripts.zsh
  ];

  extraInitExtra = builtins.foldl' (soFar: new: soFar + "\n" + builtins.readFile new) "" extras;
in
{
  programs.zsh = {
    enable = true;

    dotDir = ".config/zsh";

    enableAutosuggestions = true;
    enableCompletion = true;
    # TODO: environment.pathsToLink (see home-configuration.nix man page)

    initExtra = ''
      # for some reason the nix stuff isn't being source anymore since I
      # switched to single-user mode. So we'll just source it here to make sure
      # it's in shells by default.
      source ~/.nix-profile/etc/profile.d/nix.sh

      # from https://gist.github.com/oshybystyi/2c30543cd48b2c9ecab0
      EMOJI=(🐦 🚀 🎨 🍕 🐭 ☕️ 🔬 🐷 🐼 🐶 🐧 🐳 🍔 🍻 🔮 💰 💎 💾 💜 🍪 🌞 🌍 🐌 🐓 🐏 ✈️ )

      function random_emoji {
        echo -n "$EMOJI[$RANDOM%$#EMOJI+1]"
      }

      setopt PROMPT_SUBST

      PROMPT="$(random_emoji)  %B%F{blue}%c%f%b %F{blue}»%f "
      RPROMPT='%F{green}$(test -f .git/HEAD && sed "s|ref: refs/heads/||g" .git/HEAD)%f'

      # Although I use emacs for most development, prefer to make quick edits
      # (including stuff like git commit messages) in vim!
      EDITOR=vim
    '' + extraInitExtra;

    history = {
      # TODO: why do I need both save and size?
      save = 10000;
      size = 10000;
      share = true;
    };
  };
}
