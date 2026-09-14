{ pkgs, lib, ... }:
{
  homebrew.formulae = lib.mkIf pkgs.stdenv.hostPlatform.isDarwin [ "anki" ];

  home.packages = lib.mkIf (!pkgs.stdenv.hostPlatform.isDarwin) [ pkgs.anki ];
}
