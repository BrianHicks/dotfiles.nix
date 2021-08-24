{ ... }: {
  programs.home-manager.enable = true;
  programs.man.enable = true;

  # make the manual available
  manual.html.enable = true;
  manual.manpages.enable = true;
}
