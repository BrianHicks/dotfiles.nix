{ pkgs, lib, ... }:
{
  homebrew.formulae = lib.mkIf pkgs.stdenv.isDarwin [ "steam" ];

  nixpkgs.config.allowUnfree = true;
  home.packages = lib.mkIf (!pkgs.stdenv.isDarwin) [ pkgs.steam ];
}
