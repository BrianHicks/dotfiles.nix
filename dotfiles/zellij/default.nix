{ pkgs, ... }:
let
  # jjui inside a jj repo, lazygit everywhere else
  vcs-ui = pkgs.writeShellScript "vcs-ui" ''
    if jj status > /dev/null 2>&1; then
      exec jjui
    else
      exec lazygit
    fi
  '';
in
{
  programs.zellij = {
    enable = true;
    enableZshIntegration = true;

    layouts.hzi = ./layouts/hzi.kdl;

    settings = {
      # https://github.com/zellij-org/zellij/issues/4148
      support_kitty_keyboard_protocol = false;
    };

    extraConfig = ''
      // programs.zellij.plugins doesn't allow custom configuration as of
      // 2026-09-23, but we need it to specify our list of special programs for
      // autolock. That may change in the future... check it!
      plugins {
        autolock location="file:${pkgs.zellijPlugins.autolock}" {
          is_enabled true
          triggers "hx|jjui|lazygit|fzf"
        }
      }

      load_plugins {
        autolock
      }

      keybinds {
        shared {
          bind "Alt z" { MessagePlugin "autolock" { payload "toggle"; }; }

          // This is a bit provisional, but I *think* I want it even when
          // locked (which practically means whenever I'm in Helix)
          bind "Alt g" { Run "${vcs-ui}" { floating true; close_on_exit true; }; }
        }

        locked {
          // allow navigation even when locked
          bind "Alt h" { MoveFocusOrTab "Left"; }
          bind "Alt j" { MoveFocusOrTab "Up"; }
          bind "Alt k" { MoveFocusOrTab "Down"; }
          bind "Alt l" { MoveFocusOrTab "Right"; }
        }

        normal {
          // Intercept "Enter" and pass through for autolock
          bind "Enter" {
            WriteChars "\u{000D}";
            MessagePlugin "autolock" {};
          }
        }

        tab {
          bind "r" { NewTab { layout "hzi"; name "hzi"; }; SwitchToMode "normal"; }
        }
      }
    '';
  };
}
