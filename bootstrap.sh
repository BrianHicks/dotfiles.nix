#!/usr/bin/env bash
set -euo pipefail

if ! test -d ~/.config/nixpkgs; then mkdir -p ~/.config/nixpkgs; fi

if ! test -h ~/.config/nixpkgs/home.nix; then ln -s $(pwd)/macbook.nix ~/.config/nixpkgs/home.nix; fi

if ! which home-manager > /dev/null; then nix-shell ./home-manager -A install; fi
