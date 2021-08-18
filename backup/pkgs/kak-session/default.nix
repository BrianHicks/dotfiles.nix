{ sources ? import ../../nix/sources.nix, nixpkgs ? import sources.nixpkgs { }
}:
with nixpkgs;
stdenv.mkDerivation {
  name = "kak-session";
  src = ./.;
  buildInputs = [ pkgs.makeWrapper ];

  installPhase = ''
    mkdir -p $out/bin
    cp kak-session.sh $out/bin/kak-session
    chmod +x $out/bin/kak-session

    wrapProgram $out/bin/kak-session --prefix PATH : ${
      pkgs.lib.makeBinPath [ pkgs.kakoune ]
    }
  '';
}
