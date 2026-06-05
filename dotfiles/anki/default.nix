{ pkgs, lib, ... }:
{
  homebrew.formulae = lib.mkIf pkgs.stdenv.isDarwin [ "anki" ];

  home.packages = lib.mkIf (!pkgs.stdenv.isDarwin) [ pkgs.anki ];
}
