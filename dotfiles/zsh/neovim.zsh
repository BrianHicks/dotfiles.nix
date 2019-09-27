## neovim shortcuts

find_and_edit() {
  if test -d .git; then
    SOURCE="$(git ls-files --others --cached --exclude-standard)"
  else
    SOURCE="$(find . -type f)"
  fi

  files="$(fzf --preview='bat --color=always --paging=never --style=changes {}' --select-1 --multi --query="$@" <<< "$SOURCE")"
  if [[ "$?" != "0" ]]; then return 1; fi
  vim $files
}

alias v=find_and_edit
