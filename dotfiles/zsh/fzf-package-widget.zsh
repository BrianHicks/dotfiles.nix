#!/usr/bin/env zsh

fzf-package-widget() {
  local project_root="$(git rev-parse --show-toplevel)"
  local project_packages="$(find "$project_root" -maxdepth 3 -name 'package.json' | grep -v "${project_root}/package.json" | sed -E 's|/package.json$||g' | sed -E "s|${project_root}/(.+)|\0:\1|")"

  local package_dest="$(fzf --delimiter : --with-nth 2 <<< "$project_packages" --height 40% --reverse --scheme=path | cut -d : -f 1)"

  if test -z "$package_dest"; then
    zle redisplay
    return 0
  fi

  local package_rel="$(xargs realpath -s --relative-to "$(pwd)" "$package_dest")"

  zle push-line # clear buffer, restored on next prompt
  BUFFER="builtin cd -- ${(q)package_rel}"
  zle accept-line
  local ret=$?
  zle reset-prompt
  return $ret
}

zle -N fzf-package-widget
bindkey -M emacs '\ed' fzf-package-widget
bindkey -M vicmd '\ed' fzf-package-widget
bindkey -M viins '\ed' fzf-package-widget
