find-test-widget() {
    local selection=$(list-python-tests | fzf --height 40% --reverse --multi)

    if [[ -n "$selection" ]]; then
        LBUFFER+="${selection//$'\n'/ }"
    fi

    zle reset-prompt
}

zle -N find-test-widget

bindkey '^I' find-test-widget
