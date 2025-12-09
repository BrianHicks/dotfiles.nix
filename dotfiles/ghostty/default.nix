{ ... }:
{
  homebrew.formulae = [ "ghostty" ];

  home.file."Library/Application Support/com.mithellh.ghostty/config".text = ''
    theme = ayu
  '';
}
