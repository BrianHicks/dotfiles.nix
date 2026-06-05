{ pkgs, lib, ... }:
{
  homebrew.formulae = lib.mkIf pkgs.stdenv.isDarwin [
    "1password"
    "1password-cli"
  ];

  nixpkgs.config.allowUnfree = true;
  home.packages = lib.mkIf (!pkgs.stdenv.isDarwin) [
    pkgs._1password-gui
    pkgs._1password-cli
  ];
}
