#!/usr/bin/env bash
set -euo pipefail

# find the git root directory for the project
ROOT=$(pwd)
while ! test -d "$ROOT/.git" && test "$ROOT" != "/"; do
  ROOT=$(dirname "$ROOT")
done

if test "$ROOT" = "/"; then
  ROOT=$(pwd)
fi

SESSION=$(basename "$ROOT" | sed 's/\./-/g')

if ! kak -l | grep -q "$SESSION"; then
  kak -d -s "$SESSION"
fi

exec kak -c "$SESSION" "${@}"
