{
  pkgs ? import <nixpkgs> { },
}:
pkgs.stdenv.mkDerivation {
  name = "homebrew-sync";
  src = ./.;
  buildInputs = [ pkgs.makeWrapper ];

  installPhase = ''
    mkdir -p $out/bin
    cp ./homebrew-sync.py $out/bin/homebrew-sync
    wrapProgram $out/bin/homebrew-sync --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.python3 ]}
  '';
}
