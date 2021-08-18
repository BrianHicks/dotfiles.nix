# Every project has little utility scripts you need to run
# occasionally. `run_script` lets you get to them quickly.
#
# - If you run it without arguments, it will open a fuzzy finder for you to
#   select a script
# - If you run it with one argument, that argument will be used as your search
#   string. If the search has a single match, it will run that immediately
#   without confirming with you.
# - If you run it with more than one argument, the first argument will be
#   used as the search string and the remaining arguments will be sent to the
#   script verbatim.
#
# In any of these situations, if you quit the fuzzy finder (ctrl-c, ctrl-g,
# or esc) the script won't run anything and
#
# If you run this inside a git repo, it will offer only checked-in scripts
# below the current directory. If you run it outside of a repo, it will offer
# executable files below the current directory.
#
# TOOD: in the `git | grep`, 100755 might be too restrictive?
run_script() {
    local SCRIPTS

    if git rev-parse --show-toplevel > /dev/null; then
      SCRIPTS="$(git ls-tree -r HEAD | grep 100755 | cut -f 2)"
    else
      SCRIPTS="$(find . -perm /111 -type f)"
    fi

    if ! SELECTED="$(fzf --select-1 --query="$1" <<< "$SCRIPTS")"; then
      return 1
    fi

    "./$SELECTED" "${@:2}"
}

alias s=run_script
