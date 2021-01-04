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

  similar-sort = import sources.similar-sort { pkgs = nixpkgs.pkgs; };
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
    (kakoune.mkPlugin {
      name = "kak-elm-imports";
      src = ../pkgs/kak-elm-imports/rc;
    })
    (kakoune.mkPlugin {
      name = "kak-open";
      src = ../pkgs/kak-open/rc;
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
    [ pkgs.shellcheck (pkgs.callPackage ../pkgs/kak-session { }) pkgs.kak-lsp ];

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
      };
    };

    extraConfig = ''
      declare-user-mode surround
      map global surround s ': surround<ret>' -docstring 'Surround'
      map global surround c ': change-surround<ret>' -docstring 'Change'
      map global surround d ': delete-surround<ret>' -docstring 'Delete'
      map global surround t ': select-surrounding-tag<ret>' -docstring 'Select tag'
      map global user s ': enter-user-mode surround<ret>' -docstring 'Surround'

      declare-user-mode window
      map global user w ': enter-user-mode window<ret>' -docstring 'Windowing'
      map global window v ': tmux-terminal-horizontal sh -c %{ kak -c $1 $(${
        similar-sort-files-cmd "$2"
      }) } -- %val{session} %val{bufname}<ret>' -docstring "vertical split with fzf"
      map global window <a-v> ': tmux-terminal-horizontal sh -c %{ kak -c $1 $2 } -- %val{session} %val{bufname} <ret>' -docstring "vertical split"

      map global window s ': tmux-terminal-vertical sh -c %{ kak -c $1 $(${
        similar-sort-files-cmd "$2"
      }) } -- %val{session} %val{bufname}<ret>' -docstring "horizontal split with fzf"
      map global window <a-s> ': tmux-terminal-vertical sh -c %{ kak -c $1 $2 } -- %val{session} %val{bufname} <ret>' -docstring "horizontal split"

      # escape with fd
      hook global InsertChar d %{ try %{
        exec -draft hH <a-k>fd<ret> d
        exec <esc>
      }}

      # automatically match opening/closing pairs like () and []
      require-module auto-pairs
      auto-pairs-enable

      # automatically create directories on save
      hook global BufWritePre .* %{ mkdir %val{bufname} }

      # Git status
      hook global WinSetOption filetype=.+ %{ git show-diff }
      hook global BufWritePost .* %{ git update-diff }
      hook global BufReload .* %{ git update-diff }
      # TODO: NormalIdle?

      # Wrapping
      map global normal = '|fmt -w $kak_opt_autowrap_column<ret>'
      map global normal <a-=> ': format<ret>: echo formatted with %opt{formatcmd}<ret>'

      # Finding
      set global grepcmd '${pkgs.ripgrep}/bin/rg --follow --with-filename --line-number'
      declare-user-mode find
      map global find f ': grep<ret>' -docstring 'Find'
      map global find : ':grep ' -docstring 'Search'
      map global find s ': find-apply-changes -force<ret>: write-all<ret>' -docstring 'Apply Changes'
      map global find n ': grep-next-match<ret>' -docstring 'Next'
      map global find p ': grep-previous-match<ret>' -docstring 'Previous'
      map global find o ': buffer *grep*<ret>' -docstring 'Open Matches'
      map global user f ': enter-user-mode find<ret>' -docstring 'Find'

      # File Browsing
      set global similar_sort_path '${similar-sort}/bin/similar-sort'
      map global normal <minus> ': open-similar<ret>'
      map global normal _ ': open-similar-buffer<ret>'

      # Git
      declare-user-mode git
      map global git a ': git add<ret>: git update-diff<ret>' -docstring 'Add File'
      map global git A ': git add --all<ret>: git update-diff<ret>' -docstring 'Add All Files'
      map global git c ':git commit -m ""<left>' -docstring 'Commit'
      map global git C ':git commit --amend --no-edit<ret>: git update-diff<ret>' -docstring 'Amend, No Edit'
      map global git r ': git update-diff<ret>' -docstring 'Refresh diff markers'
      map global user g ': enter-user-mode git<ret>' -docstring 'Git'

      # Commenting
      map global normal '#' ': comment-line<ret>'
      map global normal '<a-3>' ': comment-block<ret>'

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

      map global user r ': tree-select-parent-node<ret>' -docstring 'Select Parent'

      # clipboard handling
      # https://github.com/mawww/config/blob/43bd5cea453d629dd119d361cb237d433d09a0eb/kakrc#L61-L75
      evaluate-commands %sh{
        case $(uname) in
          Linux)  copy="xclip -i"; paste="xclip -o" ;;
          Darwin) copy="pbcopy";   paste="pbpaste"  ;;
        esac

        printf "map global user -docstring 'paste (after) from clipboard' P '!%s<ret>'\n" "$paste"
        printf "map global user -docstring 'paste (before) from clipboard' p '<a-!>%s<ret>'\n" "$paste"
        printf "map global user -docstring 'yank to clipboard' y '<a-|>%s<ret>: echo -markup %%{{Information}copied selection to clipboard}<ret>'\n" "$copy"
        printf "map global user -docstring 'replace from clipboard' R '|%s<ret>'\n" "$paste"
      }

      # selections
      map global user Z '<a-z>aZ' -docstring 'Add to selection'
      map global user a 's[^, ]+<ret>' -docstring 'Split selection into arguments'

      # LSP
      eval %sh{kak-lsp --kakoune -s $kak_session}
      set global lsp_cmd "kak-lsp -s %val{session} -vvv --log /tmp/kak-lsp.log"
      lsp-enable

      map global user l ': enter-user-mode lsp<ret>' -docstring 'LSP'

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

        # extra commands
        map buffer user i ': elm-copy-import-line<ret>' -docstring 'Copy an import line'
        map buffer user d ': execute-keys -draft y,ss)mliDebug.log<space>"<esc>Pi"<space><esc>' -docstring 'Debug selection'

        # lsp
        lsp-inline-diagnostics-enable window
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

      hook global WinSetOption filetype=sh %{
        expandtab
        set-option buffer softtabstop 2
        set-option buffer tabstop 2
        set-option buffer indentwidth 2

        hook buffer BufWritePre .* lint
      }

      hook global WinSetOption filetype=html %{
        expandtab
        set-option buffer softtabstop 2
        set-option buffer tabstop 2
        set-option buffer indentwidth 2
      }

      hook global WinSetOption filetype=json %{
        expandtab
        set-option buffer softtabstop 2
        set-option buffer tabstop 2
        set-option buffer indentwidth 2
      }
    '';
  };

  # plugins
  home.file.".config/kak/colors".source =
    "${kakoune.mkColors colors}/share/kak/colors";
  home.file.".config/kak/autoload".source =
    "${kakoune.mkPlugins plugins}/share/kak/autoload";

  # LSP
  home.file."Library/Preferences/kak-lsp/kak-lsp.toml".text = ''
    [language.elm]
    filetypes = ["elm"]
    roots = ["elm.json"]
    command = "${pkgs.elmPackages.elm-language-server}/bin/elm-language-server"

    # [language.elm.initialization_options]
    # runtime = "node"
    # elmPath = "elm"
    # elmFormatPath = "elm-format"
    # elmTestPath = "elm-test"
  '';
}
