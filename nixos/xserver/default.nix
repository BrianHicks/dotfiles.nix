{ pkgs, ... }: {
  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;

    # this is a Retina display, which x needs to know for some reason.
    dpi = 227;

    xkbOptions = "ctrl:nocaps";

    libinput = {
      enable = true;

      touchpad = {
        naturalScrolling = true;
        disableWhileTyping = true;
      };
    };
  };

  # services.picom = {
  #   enable = true;
  #   vSync = true;
  #   backend = "xr_glx_hybrid";
  # };
}
