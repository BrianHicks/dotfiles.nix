#!/usr/bin/env bash
set -euo pipefail

NIXOS_CONFIG=$(pwd)/torch/default.nix nixos-rebuild $@
