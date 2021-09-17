{ ... }: {
  services.picom = {
    enable = true;
    vSync = true;

    fade = true;
    activeOpacity = 1.0;
    inactiveOpacity = 0.95;
    fadeDelta = 3; # default 10

    opacityRules = [
      # always make terminals slightly transparent
      "95:class_g = 'Alacritty' && focused"
      "90:class_g = 'Alacritty' && !focused"
    ];

    # this was the thing that made the tearing go away!
    backend = "glx";
  };
}
