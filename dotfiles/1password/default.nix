{ pkgs, lib, ... }:
{
  homebrew.formulae = lib.mkIf pkgs.stdenv.isDarwin [
    "1password"
    "1password-cli"
  ];

  home.packages = lib.mkIf (!pkgs.stdenv.isDarwin) [
    pkgs._1password-gui
    pkgs._1password-cli
  ];
}
