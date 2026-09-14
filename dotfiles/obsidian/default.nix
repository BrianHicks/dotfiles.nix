{ pkgs, lib, ... }:
{
  homebrew.formulae = lib.mkIf pkgs.stdenv.hostPlatform.isDarwin [ "obsidian" ];
  home.packages = lib.mkIf (!pkgs.stdenv.hostPlatform.isDarwin) [ pkgs.obsidian ];
}
