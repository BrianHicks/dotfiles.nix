{ pkgs, ... }:

pkgs.stdenv.mkDerivation {
  name = "smart-gen-tags";
  buildInputs = [ pkgs.bash pkgs.universal-ctags pkgs.git ];
  src = ./.;

  buildPhase = ''
    sed '/\/usr\/bin\/env bash/aPATH=$PATH:${pkgs.git}\/bin:${pkgs.universal-ctags}\/bin' \
      < smart-gen-tags.sh \
      > smart-gen-tags

    chmod +x smart-gen-tags
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp smart-gen-tags $out/bin/smart-gen-tags
  '';
}
