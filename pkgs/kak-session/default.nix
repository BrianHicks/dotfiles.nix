{ ... }:
let
  sources = import ../../nix/sources.nix;
  nixpkgs = import sources.nixpkgs { };
in with nixpkgs;
stdenv.mkDerivation {
  name = "kak-session";

  src = ./.;

  buildPhase = ''
    cat > kak-session.sh <<EOF
    #!/usr/bin/env bash
    set -euo pipefail

    # find the git root directory for the project
    ROOT=\$(pwd)
    while ! test -d "\$ROOT/.git" && test "\$ROOT" != "/"; do
      ROOT=\$(dirname \$ROOT)
    done

    if test "\$ROOT" = "/"; then
      ROOT=\$(pwd)
    fi

    SESSION=\$(basename \$ROOT | sed 's/\./-/g')

    if ! ${pkgs.kakoune}/bin/kak -l | grep -q "\$SESSION"; then
      ${pkgs.kakoune}/bin/kak -d -s "\$SESSION"
    fi

    exec ${pkgs.kakoune}/bin/kak -c "\$SESSION" \$@
    EOF
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp kak-session.sh $out/bin/kak-session
    chmod +x $out/bin/kak-session

    ln -s $out/bin/kak-session $out/bin/kak
    chmod +x $out/bin/kak
  '';
}
