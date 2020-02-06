# kill processes easily

fuzzy_kill() {
    ps aux | fzf --multi --query="$@" | awk '{ print $2 }' | xargs kill
}

alias k=fuzzy_kill
