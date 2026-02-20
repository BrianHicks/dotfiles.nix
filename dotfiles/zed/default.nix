{ pkgs, ... }:
{
  homebrew.formulae = [
    "zed"
  ];

  # Needed as global deps for Zed in a work project
  home.packages = [
    pkgs.openjdk17
    pkgs.clojure-lsp
    pkgs.nodejs_24
  ];

  programs.zed-editor = {
    enable = true;

    package = null; # Managed by homebrew

    # names: https://github.com/zed-industries/extensions/tree/main/extensions
    extensions = [
      "nix"
      "toml"
    ];

    mutableUserDebug = false;
    mutableUserKeymaps = false;
    mutableUserSettings = false;
    mutableUserTasks = false;

    userKeymaps = [
      {
        context = "Editor && vim_mode == normal && vim_operator == none && !VimWaiting";
        bindings = {
          # TUIs
          "space k" = [
            "task::Spawn"
            { task_name = "k9s"; }
          ];

          # Git
          "space g" = [
            "task::Spawn"
            { task_name = "lazygit"; }
          ];
          "space G" = "git::Diff";
          "space c" = "git::Commit";
          "cmd-enter" = "git::Commit";
          "space p" = "git::Pull";
          "space P" = "git::Push";
          "space l" = [
            "task::Spawn"
            { task_name = "GitLab MR"; }
          ];
          "space b" = "git::Branch";
          "space s" = "git::ToggleStaged";

          # jj
          "space j" = [
            "task::Spawn"
            { task_name = "jjui"; }
          ];

          # Tasks
          "space r" = "task::Rerun";
        };
      }
      {
        context = "Editor && vim_mode == insert && !menu";
        bindings = {
          "f d" = "vim::SwitchToNormalMode";
        };
      }
      {
        context = "vim_mode == visual";
        bindings = {
          "shift-s" = "vim::PushAddSurrounds";
        };
      }
    ];

    userSettings = {
      edit_predictions = {
        disabled_globs = [ ];
        mode = "eager";
        copilot = {
          proxy = null;
          proxy_no_verify = null;
          enterprise_uri = null;
        };
        enabled_in_text_threads = false;
      };
      features = {
        edit_prediction_provider = "copilot";
      };
      agent = {
        default_profile = "write";
        inline_assistant_model = {
          provider = "zed.dev";
          model = "claude-3-7-sonnet-thinking-latest";
        };
        default_model = {
          provider = "zed.dev";
          model = "claude-sonnet-4-5";
        };
      };
      vim_mode = true;
      ui_font_size = 14;
      buffer_font_size = 14;
      buffer_line_height = "standard";
      theme = {
        mode = "system";
        light = "One Light";
        dark = "One Dark";
      };
      terminal = {
        line_height = "standard";
        option_as_meta = true;
      };
      languages = {
        Markdown = {
          show_edit_predictions = false;
        };
        YAML = {
          # By default, the yaml-language-server is enabled. However, it does not
          # deal with multi-doc YAML files well, and I encounter quite a few of
          # those in my day-to-day work. Instead of dealing with the whole buffer
          # being red squigglies, I find it better to just disable the server.
          language_servers = [ ];
        };
      };
    };

    userTasks = [
      {
        label = "lazygit";
        command = "lazygit";
        hide = "on_success";
        reveal_target = "center";
      }
      {
        label = "jjui";
        command = "jjui";
        hide = "on_success";
        reveal_target = "center";
      }
      {
        label = "k9s";
        command = "k9s";
        hide = "on_success";
      }
      {
        label = "GitLab MR";
        command = "glab.mr";
        hide = "never";
      }
    ];
  };
}
