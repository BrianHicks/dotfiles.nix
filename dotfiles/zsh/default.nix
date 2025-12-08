{ pkgs, ... }: {
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

    initContent = builtins.concatStringsSep "\n\n# ---\n\n" (map builtins.readFile [
      ./prompt.zsh
      ./nixify.zsh
    ]);
  };

  home.shellAliases = {
    # Home-manager commands
    hm = "home-manager";
    hms = "home-manager switch --flake $HOME/code/BrianHicks/dotfiles.nix#brianhicks";
    hmb = "home-manager build --flake $HOME/code/BrianHicks/dotfiles.nix#brianhicks";

    # 1Password plugins
    gh = "op plugin run -- gh";
    glab = "op plugin run -- glab";
    "glab.mr" = "glab mr create --push";

    # Navigation
    root = "cd \"$(git rev-parse --show-toplevel)\"";

    # Common shortenings
    lg = "lazygit";
  };
}
