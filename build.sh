#!/usr/bin/env bash
set -euo pipefail

HERE="$(realpath $(dirname $0))"

# TODO: restore bootstrapping logic

darwin-rebuild build -I darwin-config=$HERE/macbook.nix
