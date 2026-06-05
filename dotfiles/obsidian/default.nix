{ pkgs, lib, ... }:
{
  homebrew.formulae = lib.mkIf pkgs.stdenv.isDarwin [ "obsidian" ];
  home.packages = lib.mkIf (!pkgs.stdenv.isDarwin) [ pkgs.obsidian ];
}
