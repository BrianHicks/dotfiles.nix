{ lib, pkgs, ... }:
{
  homebrew.formulae = lib.mkIf pkgs.stdenv.isDarwin [
    "handy"
  ];
}
