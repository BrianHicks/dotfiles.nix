#!/usr/bin/env nix-shell
#!nix-shell -i bash -p nodePackages.node2nix
# shellcheck shell=bash
set -euo pipefail

cd "$(realpath "$(dirname "${0:-}")")"
node2nix --composition composition.nix -12
