{ ... }:

let
  sources = import ../../nix/sources.nix;
  nixpkgs = import sources.nixpkgs { };
in with nixpkgs;

pkgs.stdenv.mkDerivation {
  name = "similar-sort";
  buildInputs = [ pkgs.go ];
  src = ./.;

  buildPhase = ''
    env HOME=$(pwd) GOPATH=$(pwd) go build similar-sort.go
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp similar-sort $out/bin
  '';
}
