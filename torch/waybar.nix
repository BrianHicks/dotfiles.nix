{ ... }: {
  programs.waybar = {
    enable = true;
    settings = [{
      layer = "top";

      modules-left = [ "sway/workspaces" "sway/mode" ];
      modules-center = [ "sway/window" ];
      modules-right = [ "cpu" "battery" "clock" ];

      modules = {
        "sway/window".max-length = 50;

        battery = {
          format = "{capacity}% {icon}";
          format-icons = [ "" "" "" "" "" ];
        };

        clock.format = "{:%a, %d. %b  %H:%M}";

        cpu.format = "{}% ";
      };
    }];
  };
}
