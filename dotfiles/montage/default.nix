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
    mbreak = "${pkgs.montage}/bin/montage start --break";
    mlunch = "${pkgs.montage}/bin/montage start Lunch --break --duration 60";

    # meetings
    mstandup = "${pkgs.montage}/bin/montage start Standup --meeting --duration 30";
    mdonut = "${pkgs.montage}/bin/montage start Donut --meeting --duration 30";

    # pairings
    mpair = "mpair_f() { ${pkgs.montage}/bin/montage start \"pairing with \$1\" --meeting };mpair_f";
  };
}
