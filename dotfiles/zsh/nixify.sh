#!/usr/bin/env bash
nixify() {
  if [[ ! -e nix/sources.json ]]; then
    niv init
  fi

  if [[ ! -e ./.envrc ]]; then
    echo "use nix" > .envrc
    direnv allow
  fi

  if [[ ! -e shell.nix ]]; then
    (
      echo '{ ... }:'
      echo 'let'
      echo '  sources = import ./nix/sources.nix;'
      echo '  nixpkgs = import sources.nixpkgs { };'
      echo '  niv = import sources.niv { };'
      echo 'in with nixpkgs;'
      echo 'stdenv.mkDerivation {'
      echo "  name = \"$(basename "$(pwd)")\";"
      echo "  buildInputs = [ niv.niv git ];"
      echo '}'
    ) > shell.nix
    ${EDITOR:-vim} shell.nix
  fi
}
