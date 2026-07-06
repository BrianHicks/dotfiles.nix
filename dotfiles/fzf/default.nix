{ pkgs, ... }:
{
  programs.fzf = rec {
    enable = true;
    enableZshIntegration = true;

    defaultCommand = "${pkgs.fd}/bin/fd";

    fileWidget = {
      command = defaultCommand;
      options = [
        "--preview '${pkgs.bat}/bin/bat --color=always --paging=never --style=changes {}' --preview-window down"
      ];
    };

    changeDirWidget = {
      command = "${pkgs.fd}/bin/fd --type d";
      options = [ "--preview '${pkgs.tree}/bin/tree -C {} | head -200'" ];
    };
  };
}
