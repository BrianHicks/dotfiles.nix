# running scripts in projects with `script/` as a directory full of executables
run_script() {
    SCRIPT_DIR=script
    SELECTED=$(find "$SCRIPT_DIR" -perm +111 -type f | sed "s|$SCRIPT_DIR/||g" | fzf --select-1 --query="$1")

    if [[ "$?" != 0 ]]; then echo "cancelling!"; return 1; fi

    $SCRIPT_DIR/$SELECTED ${@:2}
}

alias s=run_script
