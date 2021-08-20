#!/usr/bin/env bash
set -euo pipefail

nixos-rebuild --flake . "$@"
