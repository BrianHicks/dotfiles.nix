declare-option str similar_sort_path "similar-sort"

define-command -docstring 'open files named similarly to the current buffer' open-similar %{
    tmux-terminal-horizontal sh -c %{
        set -euo pipefail
        FILE="$(git ls-files --others --cached --exclude-standard | $1 $2 | grep -v $2 | fzf --tiebreak index)"
        printf "evaluate-commands -client %s edit %s\n" "$3" "$FILE" | indiekak -p $4
    } -- %opt{similar_sort_path} %val{bufname} %val{client} %val{session}
}

define-command -docstring 'open buffers named similarly to the current buffer' open-similar-buffer %{
    tmux-terminal-horizontal sh -c %{
        set -euo pipefail
        OPTIONS=""
        for OPTION in "${@:5}"; do
          OPTIONS="$(printf "%s\n%s" "$OPTIONS" "$OPTION")"
        done

        BUFFER="$(echo "$OPTIONS" | $1 $2 | grep -v $2 | grep -ve '^$' | fzf --tiebreak index)"
        printf "evaluate-commands -client %s edit '%s'\n" "$3" "$BUFFER" | indiekak -p $4
    } -- %opt{similar_sort_path} %val{bufname} %val{client} %val{session} %val{buflist}
}
