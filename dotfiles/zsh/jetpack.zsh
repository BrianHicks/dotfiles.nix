# run jetpack commands
jetpack_dev() {
    TARGETS=ui/modules
    SELECTED=$(find "$TARGETS" -type f -name '*.js' | sed "s|$TARGETS/||g" | fzf --select-1 --query="$1")

    if [[ "$?" != 0 ]]; then echo "cancelling!"; return 1; fi

    jetpack $TARGETS/$SELECTED
}

alias jd=jetpack_dev
