{ pkgs, lib, ... }:
{
  programs.ghostty = {
    enable = true;
    enableZshIntegration = true;
    settings.theme = "ayu";

    package = lib.mkIf pkgs.stdenv.hostPlatform.isDarwin null;
  };
  homebrew.formulae = lib.mkIf pkgs.stdenv.hostPlatform.isDarwin [ "ghostty" ];

  # home.file."Library/Application Support/com.mithellh.ghostty/config".text = ''
  #   theme = ayu
  # '';
}
