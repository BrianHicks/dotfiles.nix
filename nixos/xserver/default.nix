{ pkgs, ... }: {
  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;

    # this is a Retina display: 227 DPI, but that makes everything render
    # super big! 163 is more my speed.
    dpi = 163;

    xkbOptions = "ctrl:nocaps";

    libinput = {
      enable = true;

      touchpad = {
        naturalScrolling = true;
        disableWhileTyping = true;
      };
    };
  };

  services.picom = {
    enable = true;
    vSync = true;

    # this was the thing that made the tearing go away!
    backend = "glx";
  };
}
