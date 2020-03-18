{ pkgs, lib, ... }:
let
  sources = import ../nix/sources.nix;
  nixpkgs = import sources.nixpkgs { };
  kakoune = import ../lib/kakoune.nix {
    pkgs = nixpkgs.pkgs;
    lib = nixpkgs.lib;
  };

  similar-sort = pkgs.callPackage ../pkgs/similar-sort { };
  similar-sort-files-cmd = arg:
    "git ls-files --others --cached --exclude-standard | ${similar-sort}/bin/similar-sort ${arg} | fzf --tiebreak index";

  # plugins
  pluginSources = lib.filterAttrs
    (_: source: lib.attrByPath [ "kakoune" ] "" source == "plugin") sources;
  colorSources = lib.filterAttrs
    (_: source: lib.attrByPath [ "kakoune" ] "" source == "colors") sources;

  pluginAttrs = lib.mapAttrs (name: source:
    kakoune.mkPlugin {
      name = name;
      src = source;
    }) pluginSources;
  plugins = (lib.mapAttrsToList (_: plugin: plugin) pluginAttrs) ++ [ ];

  colorAttrs = lib.mapAttrs (name: source:
    kakoune.mkColorPlugin {
      name = name;
      src = source;
    }) colorSources;
  colors = lib.mapAttrsToList (_: color: color) colorAttrs;
in {
  home.packages =
    [ pkgs.shellcheck (pkgs.callPackages ../pkgs/kak-session { }) ];

  programs.kakoune = {
    enable = true;
    config = {
      colorScheme = "lucius";
      scrollOff = {
        columns = 0;
        lines = 5;
      };
      numberLines = {
        enable = true;
        separator = ''" "'';
      };
      showMatching = true;
      ui.enableMouse = true;
      ui.assistant = "clippy";
      wrapLines = {
        enable = true;
        indent = true;
        marker = "⎁";
      };

      # hooks = [



      #   # JavaScript
      #   {
      #     name = "WinCreate";
      #     option = ".*.js";
      #     commands = ''
      #       evaluate-commands %sh{
      #         if which prettier > /dev/null; then
      #           echo 'set-option buffer formatcmd "prettier --parser=typescript"'
      #         fi
      #       }
      #     '';
      #   }

      #   # Indents
      #   {
      #     name = "WinCreate";
      #     option = ".*.rb";
      #     commands = ''
      #       expandtab
      #       set-option buffer tabstop 2
      #       set-option buffer softtabstop 2
      #     '';
      #   }

      #   # kakboard
      #   {
      #     name = "WinCreate";
      #     option = ".*";
      #     commands = "kakboard-enable";
      #   }
      # ];

      # keyMappings = [
      #   # git browsing
      #   {
      #     mode = "goto";
      #     key = "u";
      #     effect = "<esc>: git next-hunk<ret>";
      #     docstring = "next hunk";
      #   }
      #   {
      #     mode = "goto";
      #     key = "<a-u>";
      #     effect = "<esc>: git prev-hunk<ret>";
      #     docstring = "previous hunk";
      #   }

      #   # file browsing
      #   {
      #     mode = "normal";
      #     key = "_";
      #     effect =
      #       ": connect-terminal sh -c %{ ranger --choosefile=/tmp/magic-file-selector $(dirname $1); if test -f /tmp/magic-file-selector; then edit $(cat /tmp/magic-file-selector); rm /tmp/magic-file-selector; fi } -- %val{bufname}<ret>";
      #   }
      #   {
      #     mode = "normal";
      #     key = "<minus>";
      #     effect = ": connect-terminal sh -c %{ edit $(${
      #         similar-sort-files-cmd "$1"
      #       }) } -- %val{bufname}<ret>";
      #   }
      #   {
      #     mode = "normal";
      #     key = "<a-minus>";
      #     effect =
      #       ": connect-terminal sh -c %{ buffer $(buffer | ${similar-sort}/bin/similar-sort $1 | fzf --tiebreak=index) } -- %val{bufname}<ret>";
      #   }

      #   # vertical selection
      #   {
      #     mode = "user";
      #     key = "v";
      #     effect = ": vertical-selection-down<ret>";
      #     docstring = "vertical selection down";
      #   }
      #   {
      #     mode = "user";
      #     key = "<a-v>";
      #     effect = ": vertical-selection-up<ret>";
      #     docstring = "vertical selection up";
      #   }
      #   {
      #     mode = "user";
      #     key = "V";
      #     effect = ": vertical-selection-up-and-down<ret>";
      #     docstring = "vertical selection up and down";
      #   }
      # ];
    };

    extraConfig = ''
      declare-user-mode surround
      map global surround s ': surround<ret>' -docstring 'Surround'
      map global surround c ': change-surround<ret>' -docstring 'Change'
      map global surround d ': delete-surround<ret>' -docstring 'Delete'
      map global surround t ': select-surrounding-tag<ret>' -docstring 'Select tag'
      map global user s ':enter-user-mode surround<ret>' -docstring 'Surround'

      declare-user-mode window
      map global user w ': enter-user-mode window<ret>' -docstring 'Windowing'
      map global window v ': tmux-terminal-horizontal sh -c %{ kak -c $1 $(${
        similar-sort-files-cmd "$2"
      }) } -- %val{session} %val{bufname}<ret>' -docstring "vertical split with fzf"
      map global window <a-v> ': tmux-terminal-horizontal sh -c %{ kak -c $1 } -- %val{session}<ret>' -docstring "vertical split"

      map global window s ': tmux-terminal-vertical sh -c %{ kak -c $1 $(${
        similar-sort-files-cmd "$2"
      }) } -- %val{session} %val{bufname}<ret>' -docstring "horizontal split with fzf"
      map global window <a-s> ': tmux-terminal-vertical sh -c %{ kak -c $1 } -- %val{session}<ret>' -docstring "horizontal split"

      # escape with fd
      hook global InsertChar d %{ try %{
        exec -draft hH <a-k>fd<ret> d
        exec <esc>
      }}

      # automatically create directories on save
      hook global BufWritePre .* %{ mkdir %val{bufname} }

      # Git status
      hook global WinSetOption filetype=.+ %{ git show-diff }
      hook global BufWritePost .* %{ git update-diff }
      hook global BufReload .* %{ git update-diff }
      # TODO: NormalIdle?

      # Languages
      hook global WinSetOption filetype=nix %{
        expandtab
        set-option buffer tabstop 2

        # formatting
        set-option buffer formatcmd nixfmt
        hook buffer BufWritePre .* format
      }

      hook global WinSetOption filetype=elm %{
        expandtab
        set-option buffer tabstop 4

        # formatting
        set-option buffer formatcmd 'elm-format --stdin'
        hook buffer BufWritePre .* format
      }

      hook global WinSetOption filetype=haskell %{
        expandtab
        set-option buffer tabstop 2

        evaluate-commands %sh{
          if which ormolu > /dev/null; then
            echo 'set-option buffer formatcmd ormolu'
            echo 'hook buffer BufWritePre .* format'
          fi
        }
      }

      hook global WinSetOption filetype=python %{
        expandtab
        set-option buffer tabstop 4

        evaluate-commands %sh{
          if which black > /dev/null; then
            echo 'set-option buffer formatcmd "black - --quiet --fast"'
            echo 'hook buffer BufWritePre .* format'
          fi
        }
      }
    '';
  };

  # plugins
  home.file.".config/kak/colors".source =
    "${kakoune.mkColors colors}/share/kak/colors";
  home.file.".config/kak/autoload".source =
    "${kakoune.mkPlugins plugins}/share/kak/autoload";
  home.file.".config/kak/kak-tree.toml".text = "";
}
