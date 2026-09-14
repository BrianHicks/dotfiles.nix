{ pkgs, lib, ... }:
{
  homebrew.formulae = lib.mkIf pkgs.stdenv.hostPlatform.isDarwin [ "spotify" ];

  home.packages = lib.mkIf (!pkgs.stdenv.hostPlatform.isDarwin) [ pkgs.spotify ];
}
