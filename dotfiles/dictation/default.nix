{ lib, pkgs, ... }:
{
  homebrew.formulae = lib.mkIf pkgs.stdenv.hostPlatform.isDarwin [
    "handy"
  ];
}
