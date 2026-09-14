{ lib, pkgs, ... }:
{
  homebrew = lib.mkIf pkgs.stdenv.hostPlatform.isDarwin {
    taps = [ "garethgeorge/backrest-tap" ];
    formulae = [ "backrest" ];
  };

  home.packages = lib.mkIf (!pkgs.stdenv.hostPlatform.isDarwin) [ pkgs.backrest ];
}
