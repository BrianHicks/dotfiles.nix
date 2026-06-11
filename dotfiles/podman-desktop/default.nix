{ pkgs, lib, ... }:
{
  homebrew.formulae = lib.mkIf pkgs.stdenv.isDarwin [ "podman-desktop" ];
}
