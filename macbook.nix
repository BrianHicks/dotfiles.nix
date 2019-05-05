{ config, pkgs, ... }:

{
  imports = [ ./home-manager/nix-darwin ];

  environment.systemPackages = [];

  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix

  # Auto upgrade nix package and the daemon service.
  # services.nix-daemon.enable = true;
  # nix.package = pkgs.nix;

  # set NIX_PATH without the root user
  nix.nixPath = [
    "darwin-config=$HOME/dotfiles.nix/macbook.nix"
    "$HOME/.nix-defexpr/channels"
  ];

  # Create /etc/zshrc that loads the nix-darwin environment.
  # for some reason I need to add /run/current-system/sw/bin/zsh as the login
  # shell for iterm. It *looks* like it should be setting the login shell but
  # it doesn't seem to be taking effect.
  programs.zsh.enable = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 3;

  # You should generally set this to the total number of logical cores in your system.
  # $ sysctl -n hw.ncpu
  nix.maxJobs = 4;
  nix.buildCores = 4;

  home-manager = {
    useUserPackages = true;
    users.brianhicks = (import ./macbook-hm.nix);
  };
}
