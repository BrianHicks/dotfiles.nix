{ config, pkgs, ... }:

{
  imports = [ ./home-manager/nix-darwin ];

  environment.systemPackages = [];

  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix
  environment.darwinConfig = "$HOME/dotfiles.nix/macbook.nix";

  # Auto upgrade nix package and the daemon service.
  # services.nix-daemon.enable = true;
  # nix.package = pkgs.nix;

  # Create /etc/bashrc that loads the nix-darwin environment.
  # programs.bash.enable = true;
  # programs.zsh.enable = true;
  # programs.fish.enable = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 3;

  # You should generally set this to the total number of logical cores in your system.
  # $ sysctl -n hw.ncpu
  nix.maxJobs = 4;
  nix.buildCores = 4;

  users.users.brianhicks.name = "brianhicks";
  users.users.brianhicks.home = "/Users/brianhicks";
  home-manager.useUserPackages = true;
  home-manager.users.brianhicks = (import ./macbook-hm.nix);
}
