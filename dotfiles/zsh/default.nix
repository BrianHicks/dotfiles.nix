{ pkgs, ... }:
{
  home.packages = [
    pkgs.list-python-tests

    # these are more for Python typing work than anything… should they go
    # elsewhere? Not hurting anything being here for now, though.
    pkgs.mypy-error-count-score
    pkgs.tokei
    (pkgs.writeShellScriptBin "mypy-error-count" "mypy --strict . | wc -l")
  ];

  programs.zsh = {
    enable = true;
    defaultKeymap = "emacs";

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
      export EDITOR=${pkgs.neovim}/bin/nvim
    '';
  };

  home.shellAliases = {
    # Navigation
    root = "cd \"$(git rev-parse --show-toplevel)\"";

    # Fix macOS quarantine
    mark-safe = "xattr -dr com.apple.quarantine";
  };
}
