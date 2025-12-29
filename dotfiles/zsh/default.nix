{ pkgs, ... }:
{
  home.packages = [ pkgs.list-python-tests ];

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
        ./find-test.zsh
      ]
    );

    envExtra = ''
      EDITOR=${pkgs.neovim}/bin/nvim
    '';
  };

  home.shellAliases = {
    # Navigation
    root = "cd \"$(git rev-parse --show-toplevel)\"";

    # Fix macOS quarantine
    mark-safe = "xattr -dr com.apple.quarantine";
  };
}
