{ ... }:
{
  programs.zsh = {
    enable = true;

    autosuggestion.enable = true;
    enableCompletion = true;
    syntaxHighlighting.enable = true;

    history = {
      append = true;
      save = 10000;
      size = 10000;
    };

    initContent = builtins.concatStringsSep "\n\n# ---\n\n" (
      map builtins.readFile [
        ./prompt.zsh
        ./nixify.zsh
        ./local-bin.zsh
      ]
    );
  };

  home.shellAliases = {
    # 1Password plugins
    gh = "op plugin run -- gh";

    # Navigation
    root = "cd \"$(git rev-parse --show-toplevel)\"";

    # Common shortenings
    lg = "lazygit";

    # Fix macOS quarantine
    mark-safe = "xattr -dr com.apple.quarantine";
  };
}
