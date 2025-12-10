{
  pkgs ? import <nixpkgs> { },
}:
pkgs.stdenv.mkDerivation {
  name = "git-hclone";
  src = ./.;
  buildInputs = [ pkgs.makeWrapper ];

  installPhase = ''
    mkdir -p $out/bin
    cp ./git-hclone.sh $out/bin/git-hclone
    wrapProgram $out/bin/git-hclone --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.git ]}
  '';
}
