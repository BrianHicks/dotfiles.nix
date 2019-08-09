## neovim shortcuts

v() {
  if test -d .git; then
    SOURCE="$(git ls-files)"
  else
    SOURCE="$(find . -type f)"
  fi

  files="$(fzf --preview='head -$FZF_PREVIEW_LINES {}' --select-1 --multi --query="$@" <<< "$SOURCE")"
  if [[ "$?" != "0" ]]; then return 1; fi
  vim $files
}
