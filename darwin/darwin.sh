#!/usr/bin/env nix-shell
#!nix-shell -i bash -p moreutils
HERE="$(realpath $(dirname $0))"
darwin-rebuild $@ -I "darwin-config=$HERE/default.nix"
