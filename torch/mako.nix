{ pkgs, ... }: {
  programs.mako.enable = true;

  home.packages = [
    pkgs.libnotify # provides notify-send
  ];
}
