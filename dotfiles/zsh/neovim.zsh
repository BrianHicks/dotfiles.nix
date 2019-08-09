## neovim shortcuts

v() {
  if test -d .git; then
    vim $(git ls-files | fzf)
  else
    vim $(find . -type f | fzf)
  fi
}
