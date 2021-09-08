{ pkgs, ... }: {
  services.xserver = {
    enable = true;
    displayManager.sessionCommands = ''
      ${pkgs.xset}/bin/xset r rate 300 50
    '';
    libinput.naturalScrolling = true;
  };
}
