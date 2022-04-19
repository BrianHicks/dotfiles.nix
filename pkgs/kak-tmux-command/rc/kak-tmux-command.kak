declare-option -docstring "tmux pane ID to target" str tmux_command_target 1

declare-option -docstring "command to send" str tmux_command_command ""

declare-option -docstring "clear screen before sending command" bool tmux_command_clear true

define-command -override tmux-send-command -params 0.. -shell-completion -docstring "send a command to the target pane" %{
    evaluate-commands %sh{
        if test "$#" -gt 0; then
          COMMAND="$@"
        elif test -z "$kak_opt_tmux_command_command"; then
          echo "fail 'tmux command was not set. Provide it in the call to tmux-send-command or with tmux-set-command.'"
          exit
        else
          COMMAND="$kak_opt_tmux_command_command"
        fi

        COMMAND="$(echo "$COMMAND" | sed "s|BUFFILE|$kak_buffile|g" | sed "s|CURSOR_LINE|$kak_cursor_line|")"

	if test "$kak_opt_tmux_command_clear" = "true"; then
	  tmux send-keys -t "$kak_opt_tmux_command_target" "C-l" > /dev/null 2>&1
	fi

        tmux send-keys -t "$kak_opt_tmux_command_target" "$COMMAND" "Enter" > /dev/null 2>&1
    }
}

define-command -override tmux-set-command -params 1.. -shell-completion -docstring "set the command to send to tmux" %{
    set-option global tmux_command_command "%arg{@}"
}

define-command -override tmux-set-target -params 1 -shell-script-completion "tmux list-panes -F '#{pane_id} (#{pane_current_command}, #{pane_width}x#{pane_height})'" -docstring "set the tmux pane to send commands to" %{
    evaluate-commands %sh{
        printf "set-option global tmux_command_target '%s'" "$(echo "$1" | cut -d ' ' -f 1)"
    }
}
