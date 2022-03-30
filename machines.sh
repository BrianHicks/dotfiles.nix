#!/usr/bin/env bash
set -euo pipefail

case "$(uname -s)" in
  Linux)
    nixos-rebuild --flake . "$@"
    ;;

  Darwin)
    nix --experimental-features 'nix-command flakes' build ".#darwinConfigurations.$(hostname -s).system"
    if test "$@" != "build"; then
      ./result/sw/bin/darwin-rebuild --flake ".#$(hostname -s)" "$@"
    fi
    ;;

  *)
    echo "I don't know how to build on a $(uname -s) system."
    exit 1
    ;;
esac
