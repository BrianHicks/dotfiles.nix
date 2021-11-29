{ pkgs, ... }: {
  # The idea here is to set a different k9s skin for each cluster I work in. I use
  # `builtins.readFile` here instead of setting `home.file.*.source` so it doesn't
  # fail silently if I make a typo.
  #
  # TODO: make this work for Linux too, if it's annoying that it doesn't. Just
  # need to use ~/.config instead?
  home.file."Library/Preferences/k9s/backyard_skin.yml".text =
    builtins.readFile "${pkgs.k9s-skins}/snazzy.yml";

  home.file."Library/Preferences/k9s/staging_skin.yml".text =
    builtins.readFile "${pkgs.k9s-skins}/monokai.yml";

  home.file."Library/Preferences/k9s/production_skin.yml".text =
    builtins.readFile "${pkgs.k9s-skins}/red.yml";

  home.file."Library/Preferences/k9s/config.yml".text = builtins.toJSON {
    k9s = {
      refreshRate = 2;
      maxConnRetry = 5;

      enableMouse = true;

      headless = false;
      logoless = true;
      crumbsless = false;
      readOnly = false;
      noIcons = false;

      logger = {
        tail = 100;
        buffer = 5000;
        sinceSeconds = 60;
        fullScreenLogs = false;
        textWrap = false;
        showTime = false;
      };

      # we need this or k9s blows up because it can't read the map (it's nil in
      # memory. Classic.)
      clusters = { };

      thresholds = {
        cpu.critical = 90;
        cpu.warn = 70;

        memory.critical = 90;
        memory.warn = 70;
      };
    };
  };
}
