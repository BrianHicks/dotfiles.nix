{ pkgs ? import <nixpkgs> { inherit system; }, system ? builtins.currentSystem
}:

let builder = (import ./node.nix { inherit pkgs system; }).package;
in with pkgs;
stdenv.mkDerivation {
  name = "kak-ayu";
  src = ./fakesrc;

  buildPhase = ''
    ${builder}/lib/node_modules/kak-ayu/bin/build.js
  '';

  installPhase = ''
    mkdir -p $out/share/kak/colors
    cp *.kak $out/share/kak/colors
  '';
}
