{ pkgs ? import <nixpkgs> { inherit system; }, system ? builtins.currentSystem
}:

let builder = (import ./node.nix { inherit pkgs system; }).package;
in with pkgs;
stdenv.mkDerivation {
  name = "kak-ayu";
  src = ./dummy;

  buildPhase = ''
    ${builder}/lib/node_modules/kak-ayu/bin/build.js
  '';

  installPhase = ''
    mkdir $out
    cp *.kak $out/
  '';
}
