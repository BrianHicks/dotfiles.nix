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

  # nix-darwin will not touch users outside this list (but users within this
  # list will be added/removed, so be careful.) The options documentation says
  # to not put the admin or other system users in here.
  users.knownUsers = [ "brianhicks" ];

  users.users.brianhicks = {
    name = "brianhicks";
    home = "/Users/brianhicks";

    packages = [];
    # packages = [
    #   pkgs.ag
    #   pkgs.awscli
    #   pkgs.jq
    #   pkgs.pv
    #   pkgs.tree
    #   pkgs.watch

    #   # local packages. I know I could use overlays for these (cf
    #   # https://github.com/jwoudenberg/dotfiles/commit/12bd31b269b82f0dc661140b8df275ef24f41b81)
    #   # but I don't want to have to symlink into the overlays directory manually.
    #   (pkgs.callPackage ./pkgs/lorri.nix { })
    # ];

    # TODO: is `uid` OK to put in here? With "brianhicks" in knownUsers,
    # darwin-rebuild blows up without it:
    #
    #    error: The option `users.users.brianhicks.uid' is used but not defined.
    #
    # my concern is basically that it is not portable to another mac.
    uid = 501;
  };

  home-manager = {
    useUserPackages = true;
    users.brianhicks = (import ./macbook-hm.nix);
  };
}
