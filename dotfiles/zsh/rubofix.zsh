# fix rubocop errors automatically
function rubofix() {
    TOPLEVEL="$(git rev-parse --show-toplevel)"
    git diff --name-only $(git rev-parse origin/master)...$(git rev-parse HEAD) | sed "s|^|$TOPLEVEL/|g" | xargs rubocop --auto-correct
}
