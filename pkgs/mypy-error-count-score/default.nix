{
  pkgs ? import <nixpkgs> { },
}:
pkgs.stdenv.mkDerivation {
  name = "mypy-error-count-score";
  src = ./.;
  buildInputs = [ pkgs.makeWrapper ];

  installPhase = ''
    mkdir -p $out/bin
    cp ./mypy-error-count-score.py $out/bin/mypy-error-count-score
    wrapProgram $out/bin/mypy-error-count-score --prefix PATH : ${
      pkgs.lib.makeBinPath [
        pkgs.python3
        pkgs.git
      ]
    }
  '';
}
