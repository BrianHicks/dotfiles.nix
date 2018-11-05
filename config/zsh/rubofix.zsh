# fix rubocop errors automatically
function rubofix() {
    git diff --name-only $(git rev-parse origin/master)...$(git rev-parse HEAD) | xargs rubocop --auto-correct
}
