{
  pkgs ? import <nixpkgs> { },
}:
pkgs.stdenv.mkDerivation {
  name = "list-python-tests";
  src = ./.;
  buildInputs = [ pkgs.makeWrapper ];

  installPhase = ''
    mkdir -p $out/bin
    cp ./list-python-tests.py $out/bin/list-python-tests
    wrapProgram $out/bin/list-python-tests --prefix PATH : ${
      pkgs.lib.makeBinPath [
        pkgs.python3
        pkgs.fd
      ]
    }
  '';
}
