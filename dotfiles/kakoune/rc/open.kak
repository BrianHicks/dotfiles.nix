declare-option str similar_sort_path "similar-sort"

define-command -docstring 'open files named similarly to the current buffer' open-similar %{
    tmux-terminal-impl 'display-popup -E' sh -c %{
        set -euo pipefail
        cd $5

        FILE="$(git ls-files --others --cached --exclude-standard | $1 $2 | grep -Fxv "$2" | fzf --tiebreak index --preview "bat --color=always -p {}")"
        printf "evaluate-commands -client %s edit %s\n" "$3" "$FILE" | kak -p $4
    } -- %opt{similar_sort_path} %val{bufname} %val{client} %val{session} %sh{ echo $PWD }
}

define-command -docstring 'open buffers named similarly to the current buffer' open-similar-buffer %{
    tmux-terminal-impl 'display-popup -E' sh -c %{
        set -euo pipefail
        cd $5

        OPTIONS=""
        for OPTION in "${@:6}"; do
          OPTIONS="$(printf "%s\n%s" "$OPTIONS" "$OPTION")"
        done

        BUFFER="$(echo "$OPTIONS" | $1 $2 | grep -Fxv "$2" | grep -ve '^$' | fzf --tiebreak index --preview "bat --color=always -p {}")"
        printf "evaluate-commands -client %s edit '%s'\n" "$3" "$BUFFER" | kak -p $4
    } -- %opt{similar_sort_path} %val{bufname} %val{client} %val{session} %sh{ echo $PWD } %val{buflist}
}

provide-module open nop
