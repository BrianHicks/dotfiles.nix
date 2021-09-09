{ pkgs, ... }: {
  services.xserver = {
    enable = true;
    libinput.naturalScrolling = true;
  };
}
