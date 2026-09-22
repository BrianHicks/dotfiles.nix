{ ... }: {
  programs.zellij = {
    enable = true;
    enableZshIntegration = true;

    settings = {
      # https://github.com/zellij-org/zellij/issues/4148
      support_kitty_keyboard_protocol = false;
    };
  };
}
