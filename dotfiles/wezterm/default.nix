{ ... }: {
  home.file.".config/wezterm/wezterm.lua".text =
    # lua
    ''
      local wezterm = require('wezterm');

      return {
        -- anything in https://wezfurlong.org/wezterm/colorschemes/index.html
        color_scheme = "nightfox",
        window_background_opacity = 0.95,

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

        -- I never want to hear BEL, thanks
        audible_bell = "Disabled",

        -- turn off the title bar (it doesn't do a lot and takes up a row or two of terminal output)
        window_decorations = "RESIZE",

        -- copy the "normal" stuff from wezterm in, plus customizations (noted in comments)
        -- https://wezfurlong.org/wezterm/hyperlinks.html
        hyperlink_rules = {
          -- things that look like URLs
          {
            regex = "\\b\\w+://(?:[\\w.-]+)\\.[a-z]{2,15}\\S*\\b",
            format = "$0",
          },

          -- email addresses
          {
            regex = "\\b\\w+@[\\w-]+(\\.[\\w-]+)+\\b",
            format = "mailto:$0",
          },

          -- file:// URI
          {
            regex = "\\bfile://\\S*\\b",
            format = "$0",
          },
        },
      }
    '';
}
