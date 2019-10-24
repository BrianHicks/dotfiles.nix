#!/usr/bin/env bash
set -euo pipefail
cd $(dirname $(realpath $0))
make result
ROOT="$(grep -m 1 'home-manager-generation/activate' result/activate | cut -d ' ' -f 5 | xargs dirname)"
exec "$ROOT/home-path/bin/vim" $@
