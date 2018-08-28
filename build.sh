#!/usr/bin/env bash
set -euo pipefail

if ! which home-manager > /dev/null; then nix-shell ./home-manager -A install; fi

home-manager -f macbook.nix build $@
