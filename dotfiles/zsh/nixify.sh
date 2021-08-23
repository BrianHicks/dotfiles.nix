#!/usr/bin/env bash
nixify() {
  if [[ ! -e nix/sources.json ]]; then
    niv init
    niv add nmattia/niv
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
      echo '  pkgs = import sources.nixpkgs { };'
      echo '  niv = import sources.niv { };'
      echo 'in pkgs.mkShell {'
      echo "  buildInputs = with pkgs; [ niv.niv git ];"
      echo '}'
    ) > shell.nix
    ${EDITOR:-vim} shell.nix
  fi
}
