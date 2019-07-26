{ pkgs, ... }:

let lorri = pkgs.callPackage ../pkgs/lorri.nix { };
in {
  launchd.user.agents.lorri = {
    command = "lorri daemon";

    path = [ lorri pkgs.nix pkgs.gnutar ];

    serviceConfig = {
      StandardErrorPath = "/usr/local/var/log/lorri.err.log";
      StandardOutPath = "/usr/local/var/log/lorri.out.log";

      EnvironmentVariables = {
        # enable the variables below to make lorri output debug stuff
        # RUST_BACKTRACE = "full";
        # RUST_LOG = "lorri=debug";
      };

      KeepAlive = true;

      # TODO: it may be possible to activate on the below socket, but I'm not
      # going to for now.
      # /Users/brianhicks/Library/Caches/com.github.target.lorri.lorri.lorri/daemon.socket
    };
  };
}
