{ pkgs, ... }:
{
  programs.fzf = rec {
    enable = true;
    enableZshIntegration = true;

    defaultCommand = "${pkgs.fd}/bin/fd";

    fileWidgetCommand = defaultCommand;
    fileWidgetOptions = [
      "--preview '${pkgs.bat}/bin/bat --color=always --paging=never --style=changes {}' --preview-window down"
    ];

    changeDirWidgetCommand = "${pkgs.fd}/bin/fd --type d";
    changeDirWidgetOptions = [ "--preview '${pkgs.tree}/bin/tree -C {} | head -200'" ];
  };
}
