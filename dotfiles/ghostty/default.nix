{ pkgs, lib, ... }:
{
  programs.ghostty = {
    enable = true;
    enableZshIntegration = true;

    settings = {
      theme = "Adventure";
      macos-option-as-alt = true;
    };

    package = lib.mkIf pkgs.stdenv.hostPlatform.isDarwin null;
  };

  homebrew.formulae = lib.mkIf pkgs.stdenv.hostPlatform.isDarwin [ "ghostty" ];
}
