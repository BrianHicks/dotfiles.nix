#!/usr/bin/env bash
set -euo pipefail
set -x

if test -n "${@:-}"; then
    ctags --totals --append $@
elif test -d .git; then
    git ls-files --others --cached --modified --exclude-standard -z | xargs -0 ctags --totals
else
    ctags -R .
fi
