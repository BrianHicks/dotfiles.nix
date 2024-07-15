{ pkgs, ... }: {
  home.stateVersion = "23.11";

  home.username = "brianhicks";
  home.homeDirectory = "/Users/brianhicks";

  home.packages = [
    pkgs.xbar-pr-status
    pkgs.xbar-review-request-status
    pkgs.xbar-shortcut
  ];

  imports = [ ./dotfiles ];
}
