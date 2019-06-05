{ pkgs, ... }:

{
  home.packages = [
    pkgs.kakoune
  ];

  home.file.".config/kak/kakrc" = {
    text = ''
      colorscheme gruvbox
      add-highlighter global/ wrap -word
    '';
  };
}
