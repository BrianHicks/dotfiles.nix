#!/usr/bin/env bash
set -euo pipefail

REPO="${1:-}"
if test -z "$REPO"; then
  echo "usage: ${0:-} user/repo"
  exit 1
fi

ROOT="${HOME:-}/"
REPO_ROOT="${REPO_ROOT:-${ROOT}code}"

mkdir -p "$REPO_ROOT/$REPO"
git clone "$(printf 'git@github.com:%s.git' "$REPO")" "$REPO_ROOT/$REPO"
