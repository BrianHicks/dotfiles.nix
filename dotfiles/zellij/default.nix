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
      keybinds {
        shared_except "locked" {
          bind "Alt g" { Run "${vcs-ui}" { floating true; close_on_exit true; }; }
        }

        tab {
          bind "r" { NewTab { layout "hzi"; name "hzi"; }; SwitchToMode "normal"; }
        }
      }
    '';
  };
}
