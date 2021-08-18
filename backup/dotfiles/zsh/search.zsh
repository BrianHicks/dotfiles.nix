## search for files

kak-search() {
    kak-session -e "grep '$@'"
}

alias f=kak-search
