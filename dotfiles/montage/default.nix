{ pkgs, ... }: {
  home.packages = [ pkgs.montage ];

  programs.zsh.shellAliases = {
    # server managment
    mserver = "montage serve";
    mvex = "montage vex --script-dir ${./scripts}";

    # alias to get around conflict with imagemagick's `montage` binary in
    # projects where that's in the PATH
    m = "${pkgs.montage}/bin/montage";

    # tasks
    mstart = "${pkgs.montage}/bin/montage start";
    mextend = "${pkgs.montage}/bin/montage extend";
    m5 = "${pkgs.montage}/bin/montage extend --by PT5M";
    m10 = "${pkgs.montage}/bin/montage extend --by PT10M";

    # breaks
    mbreak = "${pkgs.montage}/bin/montage start --break Break";
    mlunch = "${pkgs.montage}/bin/montage start Lunch --offline --duration 60";

    # signing off for the day and weekend
    msignoff = "${pkgs.montage}/bin/montage start Offline --offline --until $(date -j -v+1d -v09H -v00M -v00S -I seconds)";
    mmonday = "${pkgs.montage}/bin/montage start Offline --offline --until $(date -j -v+monday -v09H -v00M -v00S -I seconds)";
    mtuesday = "${pkgs.montage}/bin/montage start Offline --offline --until $(date -j -v+tuesday -v09H -v00M -v00S -I seconds)";
    mwednesday = "${pkgs.montage}/bin/montage start Offline --offline --until $(date -j -v+wednesday -v09H -v00M -v00S -I seconds)";
    mthursday = "${pkgs.montage}/bin/montage start Offline --offline --until $(date -j -v+thursday -v09H -v00M -v00S -I seconds)";
    mfriday = "${pkgs.montage}/bin/montage start Offline --offline --until $(date -j -v+friday -v09H -v00M -v00S -I seconds)";

    # meetings
    mstandup = "${pkgs.montage}/bin/montage start Standup --meeting --duration 30";
    mdonut = "${pkgs.montage}/bin/montage start Donut --meeting --duration 30";
    mmeeting = "${pkgs.montage}/bin/montage start --meeting";

    # pairings
    mpair = "mpair_f() { ${pkgs.montage}/bin/montage start \"pairing with \$1\" --meeting };mpair_f";
  };
}
