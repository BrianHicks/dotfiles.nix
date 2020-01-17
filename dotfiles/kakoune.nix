{ pkgs, lib, ... }:
let
  sources = import ../nix/sources.nix;
  nixpkgs = import sources.nixpkgs { };
  kakoune = import ../lib/kakoune.nix {
    pkgs = nixpkgs.pkgs;
    lib = nixpkgs.lib;
  };

  similar-sort = pkgs.callPackage ../pkgs/similar-sort { };

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
  plugins = lib.mapAttrsToList (_: plugin: plugin) pluginAttrs;

  colorAttrs = lib.mapAttrs (name: source:
    kakoune.mkColorPlugin {
      name = name;
      src = source;
    }) colorSources;
  colors = lib.mapAttrsToList (_: color: color) colorAttrs;
in {
  home.packages = [ pkgs.shellcheck ];

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

      hooks = [
        {
          commands = "auto-pairs-enable";
          name = "WinSetOption";
          option = ".*";
        }
        {
          commands = "mkdir-buffer";
          name = "BufWritePre";
          option = ".*";
        }

        # Git Status
        {
          commands = "git show-diff";
          name = "BufOpenFile";
          option = ".*";
        }
        {
          commands = "git show-diff";
          name = "WinSetOption";
          option = ".*";
        }
        {
          commands = "git update-diff";
          name = "BufWritePost";
          option = ".*";
        }
        {
          commands = "git update-diff";
          name = "BufReload";
          option = ".*";
        }

        # Nix
        {
          commands = "set-option buffer formatcmd nixfmt";
          name = "WinSetOption";
          option = ".*.nix";
        }
        {
          commands = "format";
          name = "BufWritePre";
          option = ".*.nix";
        }

        # Elm
        {
          name = "WinSetOption";
          option = ".*.elm";
          commands = ''
            evaluate-commands %sh{
              if which elm-format > /dev/null; then
                echo 'set-option buffer formatcmd "elm-format --stdin"'
              fi
            }
          '';
        }
        {
          commands = "format";
          name = "BufWritePre";
          option = ".*.elm";
        }
      ];

      keyMappings = [
        {
          mode = "normal";
          key = "<c-t>";
          effect = ": fzf-mode<ret>";
        }

        # git browsing
        {
          mode = "goto";
          key = "u";
          effect = "<esc>: git next-hunk<ret>";
          docstring = "next hunk";
        }
        {
          mode = "goto";
          key = "<a-u>";
          effect = "<esc>: git prev-hunk<ret>";
          docstring = "previous hunk";
        }

        # file browsing
        {
          mode = "normal";
          key = "_";
          effect =
            ": connect-terminal sh -c %{ ranger --choosefile=/tmp/magic-file-selector $(dirname $1); if test -f /tmp/magic-file-selector; then edit $(cat /tmp/magic-file-selector); rm /tmp/magic-file-selector; fi } -- %val{bufname}<ret>";
        }
        {
          mode = "normal";
          key = "<minus>";
          effect =
            ": connect-terminal sh -c %{ edit $(git ls-files --others --cached --exclude-standard | ${similar-sort}/bin/similar-sort $1 | fzf --tiebreak index) } -- %val{bufname}<ret>";
        }
        {
          mode = "normal";
          key = "<a-minus>";
          effect =
            ": connect-terminal sh -c %{ buffer $(buffer | ${similar-sort}/bin/similar-sort $1 | fzf --tiebreak=index) } -- %val{bufname}<ret>";
        }

        # vertical selection
        {
          mode = "user";
          key = "v";
          effect = ": vertical-selection-down<ret>";
          docstring = "vertical selection down";
        }
        {
          mode = "user";
          key = "<a-v>";
          effect = ": vertical-selection-up<ret>";
          docstring = "vertical selection up";
        }
        {
          mode = "user";
          key = "V";
          effect = ": vertical-selection-up-and-down<ret>";
          docstring = "vertical selection up and down";
        }
      ];
    };

    extraConfig = ''
      declare-user-mode surround
      map global surround s ': surround<ret>' -docstring 'Surround'
      map global surround c ': change-surround<ret>' -docstring 'Change'
      map global surround d ': delete-surround<ret>' -docstring 'Delete'
      map global surround t ': select-surrounding-tag<ret>' -docstring 'Select tag'
      map global user s ':enter-user-mode surround<ret>' -docstring 'Surround'

      # escape with fd
      hook global InsertChar d %{ try %{
        exec -draft hH <a-k>fd<ret> d
        exec <esc>
      }}
    '';
  };

  # plugins
  home.file.".config/kak/colors".source =
    "${kakoune.mkColors colors}/share/kak/colors";
  home.file.".config/kak/autoload".source =
    "${kakoune.mkPlugins plugins}/share/kak/autoload";
}
