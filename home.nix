{ config, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "brianhicks";
  home.homeDirectory = "/Users/brianhicks";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "25.11"; # Please read the comment before changing.

  # packages that I always want but which don't require any config.
  home.packages = with pkgs; [
    jq
  ];

  imports = [
    ./dotfiles/git
    ./dotfiles/fzf
  ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
