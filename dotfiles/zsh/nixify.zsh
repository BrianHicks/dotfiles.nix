#!/usr/bin/env bash
nixify() {
  if ! git status > /dev/null 2>&1; then
    git init
  fi

  if ! test -e flake.nix; then
    (
      echo '{'
      echo '  inputs = {'
      echo '    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";'
      echo '    flake-utils.url = "github:numtide/flake-utils";'
      echo '  };'
      echo ''
      echo '  outputs = inputs:'
      echo '    inputs.flake-utils.lib.eachDefaultSystem (system:'
      echo '      let pkgs = import inputs.nixpkgs { inherit system; };'
      echo '      in { '
      echo '        formatter = pkgs.nixpkgs-fmt;'
      echo ''
      echo '        devShell = pkgs.mkShell { packages = [ ]; };'
      echo '      }'
      echo '    );'
      echo '}'
    ) > flake.nix
  fi

  if ! grep -q result .gitignore > /dev/null 2>&1; then
    echo "/result" >> .gitignore
  fi

  if ! test -e .envrc; then
    echo "use flake || use nix" > .envrc
    direnv allow
  fi

  ${EDITOR:-nvim} flake.nix

  # we need to add at least flake.nix so that Nix can see it
  git add flake.nix
}
