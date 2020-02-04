#!/usr/bin/env nix-shell
#!nix-shell -i bash -p vgo2nix curl cacert gnutar jq
set -euo pipefail
set -x

here=$(dirname $(realpath $0))

tmp=$(mktemp -d)
finish() { rm -rf $tmp; }
trap finish EXIT

package=$(jq -r '.["gh-cli"].url' $here/../../nix/sources.json)
if test -f $here/deps.nix; then cp $here/deps.nix $tmp/deps.nix; fi

curl -sSL $package | tar -xz -C $tmp --strip-components=1

vgo2nix -dir $tmp
cp $tmp/deps.nix $here/deps.nix
