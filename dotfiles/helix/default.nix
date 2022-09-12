{ pkgs, ... }: {
  programs.helix = {
    enable = true;
    languages = [{
      name = "rust";
      auto-format = true;
    }];

    settings.keys.insert.f.d = "normal_mode";
  };
}
