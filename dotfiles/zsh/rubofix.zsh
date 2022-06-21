# fix rubocop errors automatically
function rubofix() {
    TOPLEVEL="$(git rev-parse --show-toplevel)"
    git diff --name-status "$(git rev-parse origin/master)...$(git rev-parse HEAD)" \
        | grep -vE '^D' \
        | grep -E '.rb$' \
        | cut -c 3- \
        | sed "s|^|$TOPLEVEL/|g" \
        | xargs rubocop --autocorrect
}
