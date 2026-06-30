{ pkgs, lib, ... }: {
  nixpkgs.config.allowUnfree = lib.mkIf pkgs.stdenv.isDarwin true;
  home.packages = [ pkgs.nomad ];
}
