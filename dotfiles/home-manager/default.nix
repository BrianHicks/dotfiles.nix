{ ... }: {
  programs.home-manager.enable = true;
  programs.man.enable = true;

  manual.html.enable = true; # adds home-manager-help
  manual.manpages.enable = true;

  # Note: this probably doesn't need to change very often!
  home.stateVersion = "21.05";
}
