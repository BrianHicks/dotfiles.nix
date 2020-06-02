{ pkgs, lib, ... }:
let
  sources = import ../nix/sources.nix;
  nixpkgs = import sources.nixpkgs { };
  kakoune = import ../lib/kakoune.nix {
    pkgs = nixpkgs.pkgs;
    lib = nixpkgs.lib;
  };

  kak-tree = pkgs.callPackage ../pkgs/kak-tree { };
  kak-ayu = pkgs.callPackage ../pkgs/kak-ayu { };

  similar-sort = pkgs.callPackage ../pkgs/similar-sort { };
  similar-sort-files-cmd = arg:
    "git ls-files --others --cached --exclude-standard | ${similar-sort}/bin/similar-sort ${arg} | grep -v ${arg} | fzf --tiebreak index";

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
  colors = (lib.mapAttrsToList (_: color: color) colorAttrs) ++ [
    (kakoune.mkColorPlugin {
      name = "ayu";
      src = kak-ayu;
    })
  ];
in {
  home.packages =
    [ pkgs.shellcheck (pkgs.callPackage ../pkgs/kak-session { }) ];

  programs.kakoune = {
    enable = true;
    config = {
      colorScheme = "ayu-mirage";
      scrollOff = {
        columns = 0;
        lines = 5;
      };
      numberLines = {
        enable = true;
        highlightCursor = true;
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

      keyMappings = [
        # file browsing
        {
          mode = "normal";
          key = "<minus>";
          effect = ": connect-terminal sh -c %{ edit $(${
              similar-sort-files-cmd "$1"
            }) } -- %val{bufname}<ret>";
        }
        {
          mode = "normal";
          key = "_";
          effect =
            ": connect-terminal sh -c %{ buffer $(buffer | ${similar-sort}/bin/similar-sort $1 | grep -v $1 | fzf --tiebreak=index) } -- %val{bufname}<ret>";
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

      # Clipboard
      hook global WinCreate .* kakboard-enable

      # Wrapping
      map global normal = '|fmt -w $kak_opt_autowrap_column<ret>'

      # Finding
      set global grepcmd '${pkgs.ripgrep}/bin/rg --follow --with-filename --line-number'
      declare-user-mode find
      map global find f ': grep<ret>' -docstring 'Find'
      map global find : ':grep ' -docstring 'Search'
      map global find s ': find-apply-changes -force<ret>: write-all<ret>' -docstring 'Apply Changes'
      map global find n ': grep-next-match<ret>' -docstring 'Next'
      map global find p ': grep-previous-match<ret>' -docstring 'Previous'
      map global find o ': buffer *grep*<ret>' -docstring 'Open Matches'
      map global user f ':enter-user-mode find<ret>' -docstring 'Find'

      # Git
      declare-user-mode git
      map global git a ': git add<ret>: git update-diff<ret>' -docstring 'Add File'
      map global git A ': git add --all<ret>: git update-diff<ret>' -docstring 'Add All Files'
      map global git s ': tmux-terminal-vertical ${pkgs.lazygit}/bin/lazygit<ret>' -docstring 'Status'
      map global git c ':git commit -m ""<left>' -docstring 'Commit'
      map global git C ':git commit --amend --no-edit<ret>: git update-diff<ret>' -docstring 'Amend, No Edit'
      map global git r ': git update-diff<ret>' -docstring 'Refresh diff markers'
      map global user g ':enter-user-mode git<ret>' -docstring 'Git'

      # kak-tree
      set global tree_cmd '${kak-tree.kak-tree}/bin/kak-tree'

      declare-user-mode tree
      map global user t ': enter-user-mode -lock tree<ret>' -docstring 'Tree Selection'
      map global tree h ': tree-select-parent-node<ret>' -docstring 'Parent'
      map global tree <a-l> ': tree-select-children<ret>' -docstring 'Children'
      map global tree l ': tree-select-first-child<ret>' -docstring 'First Child'
      map global tree j ': tree-select-next-node<ret>' -docstring 'Next Node'
      map global tree k ': tree-select-previous-node<ret>' -docstring 'Previous Node'
      map global tree d ': tree-select-parent-node value_declaration<ret>' -docstring 'Parent Declaration'

      # Languages
      hook global WinSetOption filetype=nix %{
        expandtab
        set-option buffer tabstop 2
        set-option buffer softtabstop 2
        set-option buffer indentwidth 2

        # formatting
        set-option buffer formatcmd nixfmt
        hook buffer BufWritePre .* format
      }

      hook global WinSetOption filetype=elm %{
        expandtab
        set-option buffer softtabstop 4
        set-option buffer tabstop 4
        set-option buffer indentwidth 4

        # formatting
        set-option buffer formatcmd 'elm-format --stdin'
        hook buffer BufWritePre .* format
      }

      hook global WinSetOption filetype=haskell %{
        expandtab
        set-option buffer softtabstop 2
        set-option buffer tabstop 2
        set-option buffer indentwidth 2

        evaluate-commands %sh{
          if which ormolu > /dev/null; then
            echo 'set-option buffer formatcmd ormolu'
            echo 'hook buffer BufWritePre .* format'
          fi
        }
      }

      hook global WinSetOption filetype=python %{
        expandtab
        set-option buffer softtabstop 4
        set-option buffer tabstop 4
        set-option buffer indentwidth 4

        evaluate-commands %sh{
          if which black > /dev/null; then
            echo 'set-option buffer formatcmd "black - --quiet --fast"'
            echo 'hook buffer BufWritePre .* format'
          fi
        }
      }

      hook global WinSetOption filetype=javascript %{
        expandtab
        set-option buffer softtabstop 2
        set-option buffer tabstop 2
        set-option buffer indentwidth 2

        evaluate-commands %sh{
          if which prettier > /dev/null; then
            echo 'set-option buffer formatcmd "prettier --parser=typescript"'
            echo 'hook buffer BufWritePre .* format'
          fi
        }
      }

      hook global WinSetOption filetype=rust %{
        expandtab
        set-option buffer softtabstop 4
        set-option buffer tabstop 4
        set-option buffer indentwidth 4

        evaluate-commands %sh{
          if which rustfmt > /dev/null; then
            echo 'set-option buffer formatcmd rustfmt'
            echo 'hook buffer BufWritePre .* format'
          fi
        }
      }

      hook global WinSetOption filetype=ruby %{
        expandtab
        set-option buffer softtabstop 2
        set-option buffer tabstop 2
        set-option buffer indentwidth 2
      }

      hook global WinSetOption filetype=ts %{
        expandtab
        set-option buffer softtabstop 2
        set-option buffer tabstop 2
        set-option buffer indentwidth 2
      }

      hook global WinSetOption filetype=terraform %{
        expandtab
        set-option buffer softtabstop 2
        set-option buffer tabstop 2
        set-option buffer indentwidth 2
        
        evaluate-commands %sh{
          if which terraform > /dev/null; then
            echo 'set-option buffer formatcmd "terraform fmt -"'
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
}
