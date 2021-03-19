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

      # for now I'm just converting /etc/sway/config's default bar to
      # home-manager's syntax
      bars = [{
        position = "top";
        statusCommand = "while date +'%Y-%m-%d %l:%M:%S %p'; do sleep 1; done";

        colors = {
          statusline = "#ffffff";
          background = "#323232";
          inactiveWorkspace = {
            border = "#32323200";
            background = "#32323200";
            text = "#5c5c5c";
          };
        };
      }];
    };
  };
}
