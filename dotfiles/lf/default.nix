{ pkgs, ... }: {
  programs.lf = {
    enable = true;
    keybindings = {
      # I find x to be a better cut, and save d for delete
      d = "delete";
      x = "cut";
    };

    previewer.source = "${pkgs.pistol}/bin/pistol";
  };
}
