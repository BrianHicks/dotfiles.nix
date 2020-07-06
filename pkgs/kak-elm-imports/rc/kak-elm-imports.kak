define-command -docstring "Jump to the imports section in an Elm file" elm-jump-to-imports %{
    try %{
        # a string like `import Foo as Bar` is in the file, so we can just
        # search for it.
        execute-keys '/import<ret>;gh'
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
