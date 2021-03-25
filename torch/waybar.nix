{ ... }: {
  programs.waybar = {
    enable = true;
    settings = [{
      layer = "top";

      modules-left = [ "sway/workspaces" "sway/mode" ];
      modules-center = [ ];
      modules-right = [ "cpu" "battery" "clock" ];

      modules = {
        battery = {
          format = "{capacity}% {icon}";
          format-icons = [ "" "" "" "" "" ];
        };

        clock.format = "{:%a, %b %d %H:%M}";

        cpu.format = "{}% ";
      };
    }];
  };
}
