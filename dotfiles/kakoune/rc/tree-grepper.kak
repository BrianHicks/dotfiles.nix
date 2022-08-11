declare-option str tree_grepper_path "tree-grepper"
declare-option str tree_grepper_fzf_path "fzf"

define-command -hidden -params 2..3 outline-jump %{
    tmux-terminal-impl 'display-popup -E' sh -c %{
        set -euo pipefail

        # what tools do we have available?
        TREE_GREPPER=${1:-tree-grepper}
        FZF=${2:-fzf}

        # what do we care about?
        FILE=$3
        FZF_QUERY=$4
        LANGUAGE=$8
        QUERY=$9

        # where do we return results?
        CLIENT=$5
        SESSION=$6

        cd $7
        # do the magic!

        EDIT_LOCATION="$("$TREE_GREPPER" --query "$LANGUAGE" "$QUERY" "$FILE" | fzf --with-nth 4.. --delimiter=: --query "$FZF_QUERY" --select-1 --preview 'bat --color=always -pp --highlight-line {2} --line-range "$(echo "x = $(echo {2}) - 7; if (x < 0) { 0 } else { x }" | bc):$(($(echo {2}) + 7))" {1}' | cut -d : -f 1-3 | tr : ' ')"
        printf "evaluate-commands -client %s edit %s\n" "$CLIENT" "$EDIT_LOCATION" | kak -p "$SESSION"
    } -- %opt{tree_grepper_path} %opt{tree_grepper_fzf_path} %val{bufname} %arg{3} %val{client} %val{session} %sh{ echo $PWD } %arg{1} %arg{2}
}

define-command -override -docstring "jump somewhere in an Elm file's definition outline" -params 0..1 outline-jump-elm %{
    outline-jump elm "(import_clause . (_) (upper_case_qid)@module) (function_declaration_left (lower_case_identifier)@function) (type_declaration (type) (upper_case_identifier)@type) (type_alias_declaration (type) (alias) (upper_case_identifier)@alias) (union_variant (upper_case_identifier)@constructor) (field_type (lower_case_identifier)@field) (lower_pattern)@pattern (union_pattern (upper_case_qid)@pattern) (exposed_type)@exposed_type (exposed_value)@exposed_value (as_clause (as) (upper_case_identifier)@module_alias) (port_annotation (port) (lower_case_identifier)@port) (field (lower_case_identifier)@field)" %arg{1}
}

define-command -override -docstring "jump somewhere in a Ruby file's definition outline" -params 0..1 outline-jump-ruby %{
    outline-jump ruby '(module name: (_) @module) (class name: (_) @class) (method name: (_) @method) (singleton_method name: (_) @method) (assignment left: (_) @assignment) (operator_assignment left: (_) @assignment) (block_parameters (identifier) @assignment) (call (identifier)@_name (argument_list . (simple_symbol)@let .) (#match? @_name "let(_it_be)?")) (call (identifier)@_name (argument_list . (string (string_content)@rspec) .) (#match? @_name "(context|describe|it)")) (call (identifier)@_name (argument_list . (simple_symbol)) (#match? @_name "(prop|const|attr_reader|attr_writer|attr_accessor)"))@field (method_parameters [ (identifier)@argument (keyword_parameter (identifier)@argument) (optional_parameter (identifier)@argument) ])' %arg{1}
}

define-command -override -docstring "jump somewhere in an Haskell file's definition outline" -params 0..1 outline-jump-haskell %{
    outline-jump haskell "(import)@import (signature)@signature (function name: (_)@function) (pat_name (_)@name) (adt (type)@data) (data_constructor)@constructor (newtype (type)@newtype) (newtype_constructor)@constructor (instance_head)@instance" %arg{1}
}

define-command -override -docstring "jump somewhere in an Rust file's definition outline" -params 0..1 outline-jump-rust %{
    outline-jump rust "(use_declaration (_)@use) (function_item (identifier)@function) (function_signature_item (identifier)@function) (struct_item (type_identifier)@struct) (field_declaration)@field (impl_item (type_identifier)@impl) (parameter)@arg (self_parameter)@arg (let_declaration (identifier)@let) (let_declaration (tuple_pattern (identifier)@let)) (enum_item (type_identifier)@enum) (enum_variant (identifier)@enum)"
}

define-command -override -docstring "jump somewhere in a Markdown file's outline" -params 0..1 outline-jump-markdown %{
    outline-jump markdown '(setext_heading)@heading (atx_heading)@heading'
}
