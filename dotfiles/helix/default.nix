{ pkgs, ... }: {
  programs.helix = {
    enable = true;
    languages = [{
      name = "rust";
      auto-format = true;
    }];
  };
}
