{ pkgs, ... }: {
  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;

    # this is a Retina display at 227 DPI, but it's way too big!
    # dpi = 163;

    xkbOptions = "ctrl:nocaps";
  };

  # services.xserver = {
  #   layout = "us";

  #   libinput = {
  #     enable = true;

  #     naturalScrolling = true;
  #     touchpad.disableWhileTyping = true;
  #   };
  # };

  # services.picom = {
  #   enable = true;
  #   vSync = true;
  #   backend = "xr_glx_hybrid";
  # };
}
