#!/usr/bin/env bash
tmux switch-client -t :lazygit >/dev/null 2>&1 || tmux new-window -k -n 'lazygit' -c '#{pane_current_path}' lazygit
