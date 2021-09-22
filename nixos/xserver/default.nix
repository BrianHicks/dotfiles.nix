{ pkgs, ... }: {
  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;

    # this is a Retina display: 227 DPI, but that makes everything render
    # super big! 163 is more my speed.
    dpi = 163;

    xkbOptions = "ctrl:nocaps";

    autoRepeatDelay = 250;
    autoRepeatInterval = 40;

    libinput = {
      enable = true;

      touchpad = {
        naturalScrolling = true;
        disableWhileTyping = true;
      };
    };
  };
}
