{ ... }: {
  imports = [ ../dotfiles ];

  # window management
  wayland.windowManager.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
  };
}
