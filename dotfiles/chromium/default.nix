{ pkgs, lib, ... }:
{
  # use ungoogled-chromium on macos since chromium is deprecated in homebrew
  homebrew.formulae = lib.mkIf pkgs.stdenv.isDarwin [ "ungoogled-chromium" ];

  programs.chromium = lib.mkIf (!pkgs.stdenv.isDarwin) {
    enable = true;
    extensions = [
      { id = "ddkjiahejlhfcafbddmgiahcphecmpfh"; }
    ];
  };
}
