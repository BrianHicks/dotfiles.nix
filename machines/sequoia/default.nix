{ config, pkgs, ... }:

{
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = [
    # https://nixos.wiki/wiki/Flakes#Installation_as_an_extra_command
    (pkgs.writeShellScriptBin "flix" ''
      exec ${pkgs.nixUnstable}/bin/nix --experimental-features "nix-command flakes" "$@"
    '')
  ];

  # allow zsh as a login shell
  environment.shells = [ pkgs.zsh ];
  programs.zsh.enable = true;

  # nix.package = pkgs.nixUnstable;
  nix.allowedUsers = [ "brianhicks" "root" ];
  nix.trustedUsers = [ "brianhicks" "root" ];

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  # You should generally set this to the total number of logical cores in your system.
  # $ sysctl -n hw.ncpu
  services.nix-daemon.enable = true;
  nix.maxJobs = 20;
  nix.buildCores = 20;
  users.nix.configureBuildUsers = true;

  # use nixbuild.net for distributed builds
  nix.distributedBuilds = true;
  nix.buildMachines = [{
    hostName = "eu.nixbuild.net";
    system = "x86_64-linux";
    maxJobs = 100;
    supportedFeatures = [ "benchmark" "big-parallel" ];
  }];
  nix.extraOptions = ''
    builders-use-substitutes = true
  '';
  programs.ssh.knownHosts = {
    nixBuild = {
      hostNames = [ "eu.nixbuild.net" ];
      publicKey =
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPIQCZc54poJ8vqawd8TraNryQeJnvH1eLpIDgbiqymM";
    };
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

      imports = [
        ../../dotfiles/alacritty
        ../../dotfiles/bat
        ../../dotfiles/cachix
        ../../dotfiles/comma
        ../../dotfiles/direnv
        ../../dotfiles/fzf
        ../../dotfiles/git
        ../../dotfiles/lf
        ../../dotfiles/hammerspoon
        ../../dotfiles/home-manager
        ../../dotfiles/hyperfine
        ../../dotfiles/jq
        ../../dotfiles/kakoune
        ../../dotfiles/k9s
        ../../dotfiles/ncdu
        ../../dotfiles/niv
        ../../dotfiles/ripgrep
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
