# jumping around between projects
jump() {
    BASE="$HOME/code"
    SELECTED=$(find "$BASE" -mindepth 2 -maxdepth 2 -type d | sed "s|$BASE/||g" | fzf --tiebreak=end --select-1 --query="$1")

    if [[ "$?" != 0 ]]; then echo "cancelling!"; return 1; fi

    cd "$BASE/$SELECTED"
}

alias j=jump
