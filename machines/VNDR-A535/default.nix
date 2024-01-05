{ config
, pkgs
, ...
}: {
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
  nix.settings.max-jobs = 12;
  nix.settings.cores = 12;
  nix.configureBuildUsers = true;

  # use nixbuild.net for distributed builds
  nix.distributedBuilds = true;
  nix.buildMachines = [
    {
      hostName = "eu.nixbuild.net";
      system = "x86_64-linux";
      maxJobs = 100;
      supportedFeatures = [ "benchmark" "big-parallel" ];
    }
  ];
  nix.extraOptions = ''
    builders-use-substitutes = true
    experimental-features = nix-command flakes
  '';
  programs.ssh.knownHosts = {
    nixBuild = {
      hostNames = [ "eu.nixbuild.net" ];
      publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPIQCZc54poJ8vqawd8TraNryQeJnvH1eLpIDgbiqymM";
    };
  };

  # https://github.com/nix-community/home-manager/issues/4026
  users.users.brianhicks.home = "/Users/brianhicks";

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
        pkgs.jless
        pkgs.meet
        pkgs.nix-tree
        pkgs.pstree
        pkgs.pv
        pkgs.xbar-pr-status
        pkgs.xbar-review-request-status
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
        ../../dotfiles/home-manager
        ../../dotfiles/htop
        ../../dotfiles/hyperfine
        ../../dotfiles/jq
        ../../dotfiles/k9s
        ../../dotfiles/lf
        ../../dotfiles/ncdu
        ../../dotfiles/neovim
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
