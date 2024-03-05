{ ... }: {
  programs.home-manager.enable = true;
  programs.man.enable = true;

  manual.html.enable = true; # adds home-manager-help
  manual.manpages.enable = true;
}
