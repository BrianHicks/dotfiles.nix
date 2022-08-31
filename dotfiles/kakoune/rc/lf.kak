declare-option str lf_path "lf"

define-command -docstring 'start navigating with lf' lf-browse %{
    tmux-terminal-impl 'display-popup -E' sh -c %{
        set -euo pipefail
        OUT="$(mktemp)"
        trap 'rm -rf $OUT' EXIT

        "$1" -selection-path "$OUT" "$2"
        if test -f "$OUT"; then
          printf "evaluate-commands -client %s 'edit \"%s\"'\n" "$3" "$(cat "$OUT")" | kak -p "$4"
        fi
    } -- %opt{lf_path} %val{buffile} %val{client} %val{session}
}
