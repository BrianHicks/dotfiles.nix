{ config, pkgs, ... }:

let
  macbook = import ./macbook.nix;
in
  {
    imports = [
      ./home-manager/nix-darwin
    ];

    # List packages installed in system profile. To search by name, run:
    # $ nix-env -qaP | grep wget
    environment.systemPackages = [];

    # Use a custom configuration.nix location.
    # $ darwin-rebuild switch -I darwin-config=$HOME/.dotfiles/macbook-darwin.nix
    environment.darwinConfig = "$HOME/dotfiles.nix/macbook-darwin.nix";

    # allow zsh as a login shell
    environment.shells = [ pkgs.zsh ];

    # Auto upgrade nix package and the daemon service.
    services.nix-daemon.enable = true;
    # nix.package = pkgs.nix;

    # Create /etc/bashrc that loads the nix-darwin environment.
    # programs.bash.enable = true;
    programs.zsh.enable = true;
    # programs.fish.enable = true;

    # Used for backwards compatibility, please read the changelog before changing.
    # $ darwin-rebuild changelog
    system.stateVersion = 4;

    # You should generally set this to the total number of logical cores in your system.
    # $ sysctl -n hw.ncpu
    nix.maxJobs = 8;
    nix.buildCores = 8;

    home-manager = {
      useUserPackages = true;
      users.brianhicks = macbook;
    };
  }
