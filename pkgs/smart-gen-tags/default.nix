{ pkgs, ... }:

pkgs.stdenv.mkDerivation {
  name = "smart-gen-tags";
  buildInputs = [ pkgs.bash pkgs.universal-ctags pkgs.git ];
  src = ./.;

  buildPhase = ''
    cat smart-gen-tags.sh \
      | sed 's|/usr/bin/git|${pkgs.git}/bin/git|g' \
      | sed 's|/usr/bin/ctags|${pkgs.universal-ctags}/bin/ctags|g' \
      > smart-gen-tags

    chmod +x smart-gen-tags
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp smart-gen-tags $out/bin/smart-gen-tags
  '';
}
