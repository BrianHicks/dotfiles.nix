{ config, pkgs, ... }:

{
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget

  # allow zsh as a login shell
  environment.shells = [ pkgs.zsh ];
  programs.zsh.enable = true;

  # nix.package = pkgs.nixUnstable;
  nix.settings.allowed-users = [ "brianhicks" "root" ];
  nix.settings.trusted-users = [ "brianhicks" "root" ];

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  # You should generally set this to the total number of logical cores in your system.
  # $ sysctl -n hw.ncpu
  services.nix-daemon.enable = true;
  nix.settings.max-jobs = 20;
  nix.settings.cores = 20;
  nix.configureBuildUsers = true;

  nix.extraOptions = ''
    builders-use-substitutes = true
    experimental-features = nix-command flakes
  '';

  homebrew = {
    enable = true;

    casks = [
      "1password"
      "alloy"
      "anki"
      "autodesk-fusion360"
      "firefox"
      "freecad"
      "google-chrome"
      "gpg-suite"
      "hammerspoon"
      "prusaslicer"
      "shortcat"
      "signal"
      "slack"
      "spotify"
      "steam"
      "wezterm"
      "xbar"
      "zoom"
      "zulip"
    ];
  };

  # dotfiles
  home-manager = {
    useUserPackages = true;
    users.brianhicks = { ... }: {
      # nixpkgs inside the Darwin home-manager is different than the nixpkgs that
      # gets sent down from the flake. Why? No clue. It's frustrating TBH; it
      # doesn't work this way on Linux! Anyway, the workaround is simple enough:
      # set the things we care about inside home-manager explicitly.
      nixpkgs.config = pkgs.config;
      nixpkgs.overlays = pkgs.overlays;

      home.sessionVariables.NIX_PATH = "nixpkgs=${pkgs.path}:$NIX_PATH";

      home.packages = [
        pkgs.xbar-pr-status
        pkgs.xbar-review-request-status
        pkgs.nix-tree
        pkgs.pstree
      ];

      imports = [
        ../../dotfiles/alacritty
        ../../dotfiles/bat
        ../../dotfiles/cachix
        ../../dotfiles/comma
        ../../dotfiles/direnv
        ../../dotfiles/dog
        ../../dotfiles/fd
        ../../dotfiles/fzf
        ../../dotfiles/git
        ../../dotfiles/hammerspoon
        ../../dotfiles/helix
        ../../dotfiles/home-manager
        ../../dotfiles/htop
        ../../dotfiles/hyperfine
        ../../dotfiles/jq
        ../../dotfiles/k9s
        ../../dotfiles/kakoune
        ../../dotfiles/lf

        # ncdu currently does not compile!
        # ../../dotfiles/ncdu
        ../../dotfiles/niv
        ../../dotfiles/ripgrep
        ../../dotfiles/sd
        ../../dotfiles/ssh
        ../../dotfiles/tmux
        ../../dotfiles/tree
        ../../dotfiles/tree-grepper
        ../../dotfiles/watch
        ../../dotfiles/wezterm
        ../../dotfiles/zsh
      ];
    };
  };
}
