{ pkgs, lib, ... }: {
  nixpkgs.config.allowUnfree = lib.mkIf pkgs.stdenv.hostPlatform.isDarwin true;
  home.packages = [ pkgs.nomad ];
}
