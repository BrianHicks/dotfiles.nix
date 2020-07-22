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
        BUFFER="$(echo "$5" | grep -oP "'.+?'" | sed -E "s/'(.+)'/\1/g" | $1 $2 | grep -v $2 | fzf --tiebreak index)"
        printf "evaluate-commands -client %s edit %s\n" "$3" "$BUFFER" | indiekak -p $4
    } -- %opt{similar_sort_path} %val{bufname} %val{client} %val{session} %val{buflist}
}
