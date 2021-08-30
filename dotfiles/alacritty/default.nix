{ ... }: {
  programs.alacritty = {
    enable = true;
    # see https://github.com/alacritty/alacritty/blob/master/alacritty.yml
    settings = { font.size = 11.0; };
  };
}
