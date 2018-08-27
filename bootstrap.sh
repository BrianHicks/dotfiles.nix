#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")"; pwd -P)"

mkdir -p ~/.config/nixpkgs
cat > ~/.config/nixpkgs/home.nix <<EOF
{
  programs.home-manager.enable = true;
  programs.home-manager.path = "$ROOT/home-manager";
}
EOF

echo "now run the following if you haven't:"
echo
echo "    nix-shell $ROOT/home-manager -A install"
echo
echo "and follow the rest of the instructions at github.com/rycee/home-manager"
