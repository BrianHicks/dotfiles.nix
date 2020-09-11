{ pkgs ? import <nixpkgs> { }, ... }:
let percollate = import ./composition.nix { inherit pkgs; };
in pkgs.stdenv.mkDerivation {
  name = "percollate";
  src = ./.;

  buildPhase = "true";

  installPhase = ''
    mkdir -p $out/bin
    cat > $out/bin/percollate <<EOF
    #!/usr/bin/env bash
    exec ${percollate.percollate}/bin/percollate --css "@page { size: letter } html { padding: 2em 5em; }" \$@
    EOF
    chmod +x $out/bin/percollate
  '';
}
