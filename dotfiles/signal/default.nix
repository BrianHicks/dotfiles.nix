{ pkgs, lib, ... }:
{
  homebrew.formulae = lib.mkIf pkgs.stdenv.isDarwin [ "signal" ];
  home.packages = lib.mkIf (!pkgs.stdenv.isDarwin) [ pkgs.signal-desktop ];
}
