#!/usr/bin/env bash
set -euo pipefail

if test -d .git; then
    /usr/bin/git ls-files --others --cached --modified --exclude-standard -z | xargs -0 /usr/bin/ctags --totals
else
    echo "I don't know how to handle non-git tag finding yet"
    # but... `ctags -R .` and then `find . -newer tags | xargs ctags` to update
    exit 1
fi
