{ pkgs, config, ... }: {
  environment.shells = [ pkgs.zsh ];

  users.users.brian = {
    isNormalUser = true;
    shell = pkgs.zsh;
    extraGroups = [
      "wheel" # Enable ‘sudo’ for the user.
      "audio"
    ];
  };

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    users.brian = import ../../dotfiles;
  };

  services.borgbackup.jobs.brian-home = {
    user = "brian";
    group = config.users.users.brian.group;

    repo = "ipi51989@ipi51989.repo.borgbase.com:repo";

    startAt = "hourly";

    paths = "/home/brian";
    exclude = [
      "/home/*/.cache"
      "/home/*/.cargo"
      "/home/*/.compose-cache"
      "/home/*/.config/chromium"
      "/home/*/.dropbox"
      "/home/*/Dropbox"
    ];
    compression = "auto,zlib,6";

    encryption.mode = "repokey-blake2";
    encryption.passCommand =
      "cat /home/brian/.ssh/borgbase-brian-home-encryption-key";

    environment.BORG_RSH = "ssh -i /home/brian/.ssh/borgbase_ed25519";

    extraCreateArgs = "--stats";
    extraPruneArgs = "--stats";

    prune.keep = {
      within = "1d";
      daily = 7;
      weekly = 4;
      monthly = -1; # keep one from each month
    };
  };
}
