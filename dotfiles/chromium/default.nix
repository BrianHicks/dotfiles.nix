{ pkgs, lib, ... }:
{
  homebrew.formulae = lib.mkIf pkgs.stdenv.hostPlatform.isDarwin [ "google-chrome" ];

  programs.chromium = lib.mkIf (!pkgs.stdenv.hostPlatform.isDarwin) {
    enable = true;
    extensions = [
      # uBlock Origin Lite
      { id = "ddkjiahejlhfcafbddmgiahcphecmpfh"; }

      # Kagi search
      { id = "cdglnehniifkbagbbombnjghhcihifij"; }
    ];
  };
}
