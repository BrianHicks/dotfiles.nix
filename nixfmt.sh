#!/usr/bin/env bash
set -euo pipefail
nix-shell \
    --pure \
    -p 'import (fetchTarball https://github.com/serokell/nixfmt/archive/e4f31f45799554ff378370256a24f606a3025b0a.tar.gz) {}' \
    --run "nixfmt $@"
