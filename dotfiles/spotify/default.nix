{ pkgs, lib, ... }:
{
  homebrew.formulae = lib.mkIf pkgs.stdenv.isDarwin [ "spotify" ];

  home.packages = lib.mkIf (!pkgs.stdenv.isDarwin) [ pkgs.spotify ];
}
