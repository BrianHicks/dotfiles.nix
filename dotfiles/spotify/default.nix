{ pkgs, lib, ... }:
{
  homebrew.formulae = lib.mkIf pkgs.stdenv.isDarwin [ "spotify" ];

  nixpkgs.config.allowUnfree = true;
  home.packages = lib.mkIf (!pkgs.stdenv.isDarwin) [ pkgs.spotify ];
}
