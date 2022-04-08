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

      -- my Kinesis layout uses left alt as a hacked hyper key, and right alt for
      -- an actual alt key everywhere. That means I want both of these to be
      -- meta/esc.
      send_composed_key_when_left_alt_is_pressed = false,
      send_composed_key_when_right_alt_is_pressed = false,
      use_dead_keys = false,

      -- fonts
      font = wezterm.font("JetBrains Mono"),

      -- flare the cursor for visual bell instead of flashing the background
      visual_bell = {
        fade_in_duration_ms = 75,
        fade_out_duration_ms = 75,
        target = "CursorColor",
      },
    }
  '';
}
