{ lib, pkgs, ... }:
{
  homebrew = lib.mkIf pkgs.stdenv.isDarwin {
    taps = [ "garethgeorge/backrest-tap" ];
    formulae = [ "backrest" ];
  };

  home.packages = lib.mkIf (!pkgs.stdenv.isDarwin) [ pkgs.backrest ];
}
