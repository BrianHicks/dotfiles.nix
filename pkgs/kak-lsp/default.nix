{ ... }:
let
  sources = import ../../nix/sources.nix;
  nixpkgs = import sources.nixpkgs {};
in with nixpkgs;
stdenv.mkDerivation {
  name = "kak-lsp";
  src = sources.kak-lsp;
  buildInputs = [ cargo ];

  buildPhase = ''
    env HOME=$(pwd) make build
  '';

  installPhase = ''
    env DESTDIR=$out PREFIX= make install
    mkdir -p $out/share/kak/autoload
    ln -s $out/share/kak-lsp/rc $out/share/kak/autoload/kak-lsp
  '';
}
