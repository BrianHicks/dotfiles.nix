{ pkgs, ... }: {
  # sources:
  #
  # - https://gvolpe.com/blog/xmonad-polybar-nixos/
  services = {
    dbus = {
      enable = true;
      packages = [ pkgs.gnome.dconf ];
    };

    xserver = {
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };

      displayManager = {
        gdm.enable = true;
        defaultSession = "none+xmonad";
      };
    };
  };
}
