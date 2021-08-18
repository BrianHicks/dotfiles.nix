## git shortcuts

checkout_branch() {
  branch="$(git branch --all | grep -v HEAD | grep -v '*' | sed 's/ //g' | fzf --preview 'git diff --color=always master...{}' --select-1 --query="$@" | sed -E 's|remotes/.+/||')"
  if [[ "$?" != "0" ]]; then return 1; fi
  git checkout "$branch"
}

alias co=checkout_branch

alias g=lazygit-window
