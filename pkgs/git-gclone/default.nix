{ pkgs ? import <nixpkgs> { } }:
pkgs.stdenv.mkDerivation {
  name = "git-gclone";
  src = ./.;
  buildInputs = [ pkgs.makeWrapper ];

  installPhase = ''
    mkdir -p $out/bin
    cp ./git-gclone.sh $out/bin/git-gclone
    wrapProgram $out/bin/git-gclone --prefix PATH : ${
      pkgs.lib.makeBinPath [ pkgs.git ]
    }
  '';
}

