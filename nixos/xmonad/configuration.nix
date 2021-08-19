{ pkgs, ... }: {
  # sources:
  #
  # - https://gvolpe.com/blog/xmonad-polybar-nixos/
  services = {
    gnome.gnome-keyring.enable = true;

    dbus = {
      enable = true;
      packages = [ pkgs.gnome.dconf ];
    };

    xserver = {
      enable = true;
      layout = "us";

      libinput = {
        enable = true;
        touchpad.disableWhileTyping = true;
      };

      displayManager.defaultSession = "none+xmonad";

      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };

      xkbOptions = "caps:ctrl_modifier";
    };
  };
}
