{ pkgs, ... }: {
  home.packages = [ pkgs.montage ];
  
  programs.zsh.shellAliases = {
    mstart = "${pkgs.montage}/bin/montage start";

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
