{ pkgs, lib, ... }:
{
  homebrew.formulae = lib.mkIf pkgs.stdenv.isDarwin [
    "1password"
    "1password-cli"
  ];

  # On Linux, this should be enabled at the system level
}
