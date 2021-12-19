{ ... }: {
  # todo: alt as esc isn't working on my Kinesis.
  # https://wezfurlong.org/wezterm/config/keys.html#macos-left-and-right-option-key
  # might be able to fix it?

  home.file.".config/wezterm/wezterm.lua".text = ''
    local wezterm = require('wezterm');

    return {
      -- anything in https://wezfurlong.org/wezterm/colorschemes/index.html
      -- another good one: BlulocoDark
      color_scheme = "Ayu Mirage",

      hide_tab_bar_if_only_one_tab = true,
    }
  '';
}
