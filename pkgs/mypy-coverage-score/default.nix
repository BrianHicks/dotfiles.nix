{
  pkgs ? import <nixpkgs> { },
}:
pkgs.stdenv.mkDerivation {
  name = "mypy-coverage-score";
  src = ./.;
  buildInputs = [ pkgs.makeWrapper ];

  installPhase = ''
    mkdir -p $out/bin
    cp ./mypy-coverage-score.py $out/bin/mypy-coverage-score
    wrapProgram $out/bin/mypy-coverage-score --prefix PATH : ${
      pkgs.lib.makeBinPath [
        pkgs.python3
        pkgs.git
      ]
    }
  '';
}
