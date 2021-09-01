define-command -docstring "Jump to the imports section in an Elm file" elm-jump-to-imports %{
    try %{
        # a string like `import Foo as Bar` is in the file, so we can just
        # search for it.
        execute-keys 'gg/import<ret>;gh'
    } catch %{
        # no import stanzas in the file, so we need to get past the module
        # declaration (which may be multiple lines) and any module docs (which
        # are almost certainly multiple lines.)
        execute-keys 'gg/exposing.+\(<ret>mjjgh'
        try %{
            # try to jump to the line past the end of the docstring
            execute-keys mj
        }
    }
}

define-command -docstring "Sort the imports in this file" elm-sort-imports %{
    execute-keys -draft '/(import.+?\n)+<ret>|sort<ret>|uniq<ret>'
}

define-command -docstring "Copy an import line from elsewhere into this file" elm-copy-import-line %{
    tmux-terminal-vertical sh -c %{
        IMPORTS=$(rg -INt elm '^import ' | sort | uniq -c | sort -rn | sed -E 's/^\s*[0-9]+\s*//g' | fzf --multi --tiebreak end,length)
        printf "execute-keys -draft -client $1 ': elm-jump-to-imports<ret>o%s<esc>: elm-sort-imports<ret>\n'" "$IMPORTS" | kak -p $2
    } -- %val{client} %val{session}
}
