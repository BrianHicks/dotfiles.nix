#!/usr/bin/env bash
set -euo pipefail

PROFILE="${1:-}"
if test -z "$PROFILE"; then
    echo "Usage: ${0:-} <profile>"
    exit 1
fi

nix run home-manager/release-25.11 -- switch --flake ".#$PROFILE"
