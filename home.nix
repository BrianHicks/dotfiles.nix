{ pkgs, ... }: {
  home.stateVersion = "23.11";

  home.packages = [
    pkgs.xbar-pr-status
    pkgs.xbar-review-request-status
  ];

  imports = [ ./dotfiles ];
}
