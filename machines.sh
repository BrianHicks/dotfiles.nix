#!/usr/bin/env bash
set -euo pipefail

case "$(uname -s)" in
  Linux)
    nixos-rebuild --flake . "$@"
    ;;

  Darwin)
    nix-shell "-p" nixUnstable --run 'nix --experimental-features "nix-command flakes" build .#darwinConfigurations.flame.system'
    if test "$@" != "build"; then
      ./result/sw/bin/darwin-rebuild --flake . "$@"
    fi
    ;;

  *)
    echo "I don't know how to build on a $(uname -s) system."
    exit 1
    ;;
esac
