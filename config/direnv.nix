{ pkgs, ... }:

{
  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
  };

  home.file.".direnvrc".text = ''
    use_nix() {
      eval "$(lorri direnv)"

      target="$(lsof -p "$$" | grep cwd | awk '{ print $9 }')"
      for pid in $(ps -o pid,command | grep 'lorri watch' | grep -v 'grep' | awk '{ print $1 }'); do
        if lsof -p "$pid" | grep cwd | grep -q "$target"; then
          echo "using existing lorri watch with pid $pid"
          return 0;
        fi
      done

      # existing lorri watch wasn't found
      lorri watch 2>/dev/null 1>/dev/null &
      lorri_pid=$!
      disown $lorri_pid
      echo "started lorri watch with pid $lorri_pid"
    }
  '';
}
