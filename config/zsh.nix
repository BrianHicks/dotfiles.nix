{ pkgs, ... }:

{
  programs.zsh = {
    enable = true;

    dotDir = ".config/zsh";

    enableAutosuggestions = true;
    enableCompletion = true;
    # TODO: environment.pathsToLink (see home-configuration.nix man page)

    initExtra = ''
      # from https://gist.github.com/oshybystyi/2c30543cd48b2c9ecab0
      EMOJI=(🐦 🚀 🎨 🍕 🐭 ☕️ 🔬 🐷 🐼 🐶 🐧 🐳 🍔 🍻 🔮 💰 💎 💾 💜 🍪 🌞 🌍 🐌 🐓 🐏 ✈️ )

      function random_emoji {
        echo -n "$EMOJI[$RANDOM%$#EMOJI+1]"
      }

      PROMPT="$(random_emoji)  %c ~ "
    '';

    history = {
      # TODO: why do I need both save and size?
      save = 10000;
      size = 10000;
      share = true;
    };
  };
}
