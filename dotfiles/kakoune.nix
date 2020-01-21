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

  kak-tree = pkgs.callPackages ../pkgs/kak-tree { };

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
  plugins = (lib.mapAttrsToList (_: plugin: plugin) pluginAttrs) ++ [
    (kakoune.mkPlugin {
      name = "kak-tree";
      src = "${kak-tree.src}/rc";
    })
  ];

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
          name = "WinCreate";
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
          name = "WinCreate";
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
          commands = ''
            set-option buffer formatcmd nixfmt
          '';
          name = "WinCreate";
          option = ".*.nix";
        }
        {
          commands = "format";
          name = "BufWritePre";
          option = ".*.nix";
        }

        # Elm
        {
          name = "WinCreate";
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

        # Indents
        {
          name = "WinCreate";
          option = ".*.(nix|rb)";
          commands = ''
            expandtab
            set-option buffer tabstop 2
            set-option buffer softtabstop 2
          '';
        }
        {
          name = "WinCreate";
          option = ".*.elm";
          commands = ''
            expandtab
            set-option buffer tabstop 4
            set-option buffer softtabstop 4
          '';
        }
      ];

      keyMappings = [
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
          effect = ": connect-terminal sh -c %{ edit $(${
              similar-sort-files-cmd "$1"
            }) } -- %val{bufname}<ret>";
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

      declare-user-mode window
      map global window v ': tmux-terminal-horizontal sh -c %{ kak -c $1 $(${
        similar-sort-files-cmd "$2"
      }) } -- %val{client_pid} $val{bufname}<ret>' -docstring "vertical split with selection"
      map global window s ': tmux-terminal-vertical sh -c %{ kak -c $1 $(${
        similar-sort-files-cmd "$2"
      }) } -- %val{client_pid} $val{bufname}<ret>' -docstring "horizontal split with selection"
      map global user w ': enter-user-mode window<ret>' -docstring 'Windowing'

      # escape with fd
      hook global InsertChar d %{ try %{
        exec -draft hH <a-k>fd<ret> d
        exec <esc>
      }}

      # kak-tree
      set global tree_cmd '${kak-tree.kak-tree}/bin/kak-tree -vvv'

      declare-user-mode tree
      map global user t ': enter-user-mode -lock tree<ret>' -docstring 'Tree Selection'
      map global tree h ': tree-select-parent-node<ret>' -docstring 'Parent'
      map global tree l ': tree-select-children<ret>' -docstring 'Children'
      map global tree <a-l> ': tree-select-first-child<ret>' -docstring 'First Child'
      map global tree j ': tree-select-next-node<ret>' -docstring 'Next Node'
      map global tree k ': tree-select-previous-node<ret>' -docstring 'Previous Node'
      map global tree ? ': tree-node-sexp<ret>' -docstring 'Show Node'
    '';
  };

  # plugins
  home.file.".config/kak/colors".source =
    "${kakoune.mkColors colors}/share/kak/colors";
  home.file.".config/kak/autoload".source =
    "${kakoune.mkPlugins plugins}/share/kak/autoload";
  home.file.".config/kak/kak-tree.toml".text = "";
}
