{ config, pkgs, ... }:

{
  imports = [ ../home-manager/nix-darwin ./defaults.nix ];

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = [ ];

  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/.dotfiles/darwin/default.nix
  environment.darwinConfig = "$HOME/code/brian/dotfiles.nix/darwin/default.nix";

  # allow zsh as a login shell
  environment.shells = [ pkgs.zsh ];

  # nix.package = pkgs.nixUnstable;
  nix.allowedUsers = [ "brianhicks" ];
  nix.trustedUsers = [ "brianhicks" ];

  # Create /etc/bashrc that loads the nix-darwin environment.
  # programs.bash.enable = true;
  programs.zsh.enable = true;
  # programs.fish.enable = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  # You should generally set this to the total number of logical cores in your system.
  # $ sysctl -n hw.ncpu
  services.nix-daemon.enable = false;
  nix.maxJobs = 8;
  nix.buildCores = 8;

  # enabling distributed builds removes a blank config line from
  # /etc/nix/nix.conf. github.com/nix-community/linuxkit-nix seems to do the
  # rest just fine.
  nix.distributedBuilds = true;

  # dotfiles
  home-manager = {
    useUserPackages = true;
    users.brianhicks = (import ../dotfiles);
  };
}
