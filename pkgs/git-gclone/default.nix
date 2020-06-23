{ sources ? import ../../nix/sources.nix, nixpkgs ? import sources.nixpkgs { }
}:
with nixpkgs;
stdenv.mkDerivation {
  name = "git-gclone";
  src = ./.;
  buildInputs = [ pkgs.makeWrapper ];

  installPhase = ''
    mkdir -p $out/bin
    cp ./git-gclone.sh $out/bin/git-gclone
    wrapProgram $out/bin/git-gclone --prefix PATH : ${
      pkgs.lib.makeBinPath [ git ]
    }
  '';
}

