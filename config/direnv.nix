{ pkgs, ... }:

{
  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
  };

  home.file.".direnvrc".text = ''
    use_nix() {
      eval "$(lorri direnv)"

      COMMAND="lorri daemon"
      LORRI_PID="$(ps -ao pid,command | grep "$COMMAND" | grep -v grep | cut -d ' ' -f 1)"

      if test -z "$LORRI_PID"; then
        lorri daemon 2>&1 1>/tmp/lorri-daemon.log &
        LORRI_PID=$!
        disown "$LORRI_PID"
        sleep 0.25
        echo "started '$COMMAND' with PID $LORRI_PID"
      else
        echo "using existing '$COMMAND' with PID $LORRI_PID"
      fi
    }
  '';
}
