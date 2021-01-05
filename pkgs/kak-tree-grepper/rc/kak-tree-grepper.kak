declare-option str tree_grepper_path "tree-grepper"
declare-option str tree_grepper_fzf_path "fzf"

define-command -docstring "jump somewhere in an Elm file's definition outline" outline-jump-elm %{
    tmux-terminal-horizontal sh -c %{
        set -euo pipefail

        # what tools do we have available?
        TREE_GREPPER=${1:-tree-grepper}
        FZF=${2:-fzf}

        # what file do we care about?
        FILE=$3

        # where do we return results?
        CLIENT=$4
        SESSION=$5

        # do the magic!
        QUERY="(function_declaration_left (lower_case_identifier)@function) (type_declaration (type) (upper_case_identifier)@type) (type_alias_declaration (type) (alias) (upper_case_identifier)@alias) (union_variant (upper_case_identifier)@constructor) (field_type (lower_case_identifier)@field)"
        # TODO: maybe also (import_clause (import) (upper_case_qid)@import)?

        EDIT_LOCATION="$("$TREE_GREPPER" "$QUERY" "$FILE" | fzf --with-nth 4,5 --nth 2,1 --delimiter=: | cut -d : -f 1-3 | tr : ' ')"
        printf "evaluate-commands -client %s edit %s\n" "$CLIENT" "$EDIT_LOCATION" | indiekak -p "$SESSION"
    } -- %opt{tree_grepper_path} %opt{tree_grepper_fzf_path} %val{bufname} %val{client} %val{session}
}
