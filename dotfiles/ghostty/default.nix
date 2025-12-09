{ ... }:
{
  homebrew.formulae = [ "ghostty" ];

  home.file."Library/Application Support/com.mithellh.ghostty/config" = ''
    theme = ayu
  '';
}
