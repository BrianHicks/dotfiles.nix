{ pkgs, lib, ... }: {
  wayland.windowManager.sway = {
    enable = true;
    config = let modifier = "Mod4";
    in {
      modifier = modifier;
      keybindings = lib.mkOptionDefault {
        "${modifier}+Return" = "exec ${pkgs.alacritty}/bin/alacritty";
        "${modifier}+d" = "exec ${pkgs.rofi}/bin/rofi -show drun";

        # Audio
        # note: get special key names with `xev -event keyboard`
        "XF86AudioRaiseVolume" =
          "exec pactl set-sink-volume @DEFAULT_SINK@ +5%";
        "XF86AudioLowerVolume" =
          "exec pactl set-sink-volume @DEFAULT_SINK@ -5%";
        "XF86AudioMute" = "exec pactl set-sink-mute @DEFAULT_SINK@ toggle";
        "XF86AudioPlay" = "exec ${pkgs.playerctl}/bin/playerctl play-pause";
        "XF86AudioNext" = "exec ${pkgs.playerctl}/bin/playerctl next";
        "XF86AudioPrev" = "exec ${pkgs.playerctl}/bin/playerctl previous";

        # Brightness
        "XF86MonBrightnessDown" = "exec brillo -q -U 5";
        "XF86MonBrightnessUp" = "exec brillo -q -A 5";
        "XF86KbdBrightnessDown" = "exec brillo -q -k -U 5";
        "XF86KbdBrightnessUp" = "exec brillo -q -k -A 5";
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

      bars = [{ command = "waybar"; }];
    };

    extraConfig = ''
      output "Apple Computer Inc Color LCD 0x00000000" scale 1.6
      output * background ${../wallpapers/alien-moon.png} fill

      exec mako

      # Start user units
      # TODO: is this right? Is there a better way to start them?
      exec systemctl start --user gammastep
      exec systemctl start --user pulseaudio
      exec systemctl start --user ssh-agent
    '';
  };

  xdg.configFile."environment.d/ssh-agent.conf".text = ''
    SSH_AUTH_SOCK=''${XDG_RUNTIME_DIR}/ssh-agent.socket
  '';
}
