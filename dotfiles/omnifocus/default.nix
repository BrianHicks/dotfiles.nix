{ pkgs, lib, ... }: {
  homebrew.formulae = lib.mkIf pkgs.stdenv.hostPlatform.isDarwin [
    "omnifocus"
  ];
}
