{ ... }: {
  services.picom = {
    enable = true;
    vSync = true;

    fade = true;
    activeOpacity = 1.0;
    inactiveOpacity = 0.9;
    fadeDelta = 3; # default 10

    # this was the thing that made the tearing go away!
    backend = "glx";
  };
}
