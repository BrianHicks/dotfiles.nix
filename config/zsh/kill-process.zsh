# killing processes
# ripped off https://github.com/SidOfc/dotfiles/blob/d07fa3862ed065c2a5a7f1160ae98416bfe2e1ee/zsh/kp
kill-process() {
    local PROCESSES="$(ps aux | sed 1d | fzf -em | awk '{print $1}')"

    if test -n "$PROCESSES"; then
        echo "$PROCESSES" | xargs kill -${1:-9}
        kill-process
    fi
}

alias kp=kill-process
