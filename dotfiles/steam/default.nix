{ pkgs, lib, ... }:
{
  homebrew.formulae = lib.mkIf pkgs.stdenv.isDarwin [ "steam" ];

  home.packages = lib.mkIf (!pkgs.stdenv.isDarwin) [ pkgs.steam ];
}
