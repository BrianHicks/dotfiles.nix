{ ... }:
let
  # The idea here is to set a different k9s skin for each cluster I work in. I use
  # `builtins.readFile` here instead of setting `home.file.*.source` so it doesn't
  # fail silently if I make a typo.
  config = builtins.toJSON {
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
        textWrap = true;
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
in
{
  # for non-work contexts, I use a version of k9s that uses the macOS system
  # directories, but in work contexts I use an (older) version that uses
  # ~/.k9s. Gotta have both for my skins to work!
  home.file.".k9s/config.yml".text = config;
  home.file."Library/Application Support/k9s/config.yml".text = config;
}
