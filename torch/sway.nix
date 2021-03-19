{ ... }: {
  wayland.windowManager.sway = {
    enable = true;
    config = {
      modifier = "Mod4";

      # this is the default and I'm trying it for now but I'm not completely sold
      focus.followMouse = "yes";

      # trackpad
      input."1452:627:bcm5974" = {
        tap = "enabled";
        natural_scroll = "enabled";
      };

      input."type:keyboard" = {
        repeat_rate = "40"; # characters per second
        repeat_delay = "200"; # milliseconds
        xkb_options = "ctrl:nocaps"; # caps lock is ctrl
      };
    };
  };
}
