{ pkgs, lib, ... }: {
  wayland.windowManager.sway = {
    enable = true;
    config = let modifier = "Mod4";
    in {
      modifier = modifier;
      keybindings = lib.mkOptionDefault {
        "${modifier}+Return" = "exec ${pkgs.alacritty}/bin/alacritty";
      };

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

    extraConfig = ''
      output "Apple Computer Inc Color LCD 0x00000000" scale 1.5

      # Start the pulseaudio unit so audio works when the system starts. TODO:
      # is this right? it feels like there's probably a better way to start it?
      exec systemctl start --user pulseaudio
    '';
  };
}