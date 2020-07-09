## search for files

kak-search() {
    kak -e "grep '$@'"
}

alias f=kak-search
