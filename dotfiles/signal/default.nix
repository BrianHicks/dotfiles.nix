{ pkgs, lib, ... }:
{
  homebrew.formulae = lib.mkIf pkgs.stdenv.isDarwin [ "signal" ];
  home.packags = lib.mkIf (!pkgs.stdenv.isDarwin) [ pkgs.signal-desktop ];
}
