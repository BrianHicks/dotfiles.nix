#!/usr/bin/env bash
set -euo pipefail

REPO="${1:-}"
if test -z "$REPO"; then
  echo "usage: ${0:-} path/to/repo"
  exit 1
fi

HZ_GITLAB="${HZ_GITLAB:-}"
if test -z "$HZ_GITLAB"; then
  echo "HZ_GITLAB is unset; set before cloning"
  exit 1
fi

HZ_EMAIL="${HZ_EMAIL:-}"
if test -z "$HZ_EMAIL"; then
  echo "HZ_EMAIL is unset; set before cloning"
  exit 1
fi

ROOT="${HOME:-}/"
REPO_ROOT="${REPO_ROOT:-${ROOT}code}"

TARGET="$REPO_ROOT/$REPO"

mkdir -p "$TARGET"
git clone "$(printf 'ssh://git@%s/%s.git' "$HZ_GITLAB" "$REPO")" "$TARGET"
(cd "$TARGET"; git config user.email "$HZ_EMAIL")
