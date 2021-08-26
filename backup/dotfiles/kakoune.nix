{ pkgs, lib, ... }:
let
  sources = import ../nix/sources.nix;
  nixpkgs = import sources.nixpkgs { };
  kakoune = pkgs.callPackage ../lib/kakoune.nix { };

  kak-tree = pkgs.callPackage ../pkgs/kak-tree { };
  kak-ayu = pkgs.callPackage ../pkgs/kak-ayu { };
  kak-lsp = pkgs.callPackage ../pkgs/kak-lsp { };

  similar-sort = pkgs.callPackage sources.similar-sort { };
  similar-sort-files-cmd = arg:
    "git ls-files --others --cached --exclude-standard | ${similar-sort}/bin/similar-sort ${arg} | grep -v ${arg} | fzf --tiebreak index";

  tree-grepper = pkgs.callPackage ../pkgs/tree-grepper { };

  kak-subvert = "${pkgs.callPackage ../pkgs/kak-subvert { }}/bin/kak-subvert";

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
    (kakoune.mkPlugin {
      name = "kak-tree-grepper";
      src = ../pkgs/kak-tree-grepper/rc;
    })
    (kakoune.mkPlugin {
      name = "auto-pairs.kak";
      src = ../vendor/auto-pairs.kak/rc;
    })
    ((kakoune.mkPlugin {
      name = "kakoune-idris";
      src = sources.kakoune-idris;
    }).overrideAttrs (attrs: {
      patches = [
        (builtins.fetchurl
          "https://patch-diff.githubusercontent.com/raw/stoand/kakoune-idris/pull/8.patch")
        (builtins.fetchurl
          "https://patch-diff.githubusercontent.com/raw/stoand/kakoune-idris/pull/9.patch")
      ];
    }))
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

  copyCommand = if pkgs.stdenv.isDarwin then
    "pbcopy"
  else
    "${pkgs.wl-clipboard}/bin/wl-copy";

  pasteCommand = if pkgs.stdenv.isDarwin then
    "pbpaste"
  else
    "${pkgs.wl-clipboard}/bin/wl-paste";
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

      # automatically match opening/closing pairs like () and []
      require-module auto-pairs
      auto-pairs-enable

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

      map global goto n '<esc>: git next-hunk<ret>' -docstring 'Next hunk'
      map global goto N '<esc>: git prev-hunk<ret>' -docstring 'Prev hunk'

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

      # kak-subvert
      declare-user-mode subvert
      map global user c ': enter-user-mode subvert<ret>' -docstring 'case changes'
      map global subvert c '<esc> | ${kak-subvert} camel <ret>' -docstring 'convert to camelCase'
      map global subvert k '<esc> | ${kak-subvert} kebab <ret>' -docstring 'convert to kebab-case'
      map global subvert p '<esc> | ${kak-subvert} pascal <ret>' -docstring 'convert to PascalCase'
      map global subvert . '<esc> | ${kak-subvert} sentence <ret>' -docstring 'convert to Sentence case'
      map global subvert s '<esc> | ${kak-subvert} snake <ret>' -docstring 'convert to snake_case'
      map global subvert S '<esc> | ${kak-subvert} screaming <ret>' -docstring 'convert to SCREAMING_CASE'
      map global subvert t '<esc> | ${kak-subvert} train <ret>' -docstring 'convert to Train-Case'
      map global subvert T '<esc> | ${kak-subvert} title <ret>' -docstring 'convert to Title Case'
      map global subvert u '<esc> | ${kak-subvert} ugly <ret>' -docstring 'convert to Ugly_Case'

      # clipboard handling
      # inspired by https://github.com/mawww/config/blob/43bd5cea453d629dd119d361cb237d433d09a0eb/kakrc#L61-L75
      map global user -docstring 'paste (after) from clipboard' P '!${pasteCommand}<ret>'
      map global user -docstring 'paste (before) from clipboard' p '<a-!>${pasteCommand}<ret>'
      map global user -docstring 'yank to clipboard' y '<a-|>${copyCommand}<ret>: echo -markup %{{Information}copied selection to clipboard}<ret>'
      map global user -docstring 'replace from clipboard' R '|${pasteCommand}<ret>'

      # selections
      map global user Z '<a-z>aZ' -docstring 'Add to selection'
      map global user a 's[^, ]+<ret>' -docstring 'Split selection into arguments'

      # outline jumping
      set global tree_grepper_path "${tree-grepper}/bin/tree-grepper"
      set global tree_grepper_fzf_path "${pkgs.fzf}/bin/fzf"

      # language server
      # eval %sh{${kak-lsp}/bin/kak-lsp --config ~/.config/kak-lsp/kak-lsp.toml --kakoune -s $kak_session}
      # map global user l ': enter-user-mode lsp<ret>' -docstring 'LSP'

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

        map buffer normal <a-minus> ': outline-jump-elm<ret>'
        map buffer goto d '<a-i>w: outline-jump-elm %val{selection}<ret>' -docstring 'Def (same file)'
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

        map buffer normal <a-minus> ': outline-jump-haskell<ret>'
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

        map buffer normal <a-minus> ': outline-jump-rust<ret>'

        # lsp-enable-window

        # hook window -group rust-inlay-hints BufReload .* rust-analyzer-inlay-hints
        # hook window -group rust-inlay-hints NormalIdle .* rust-analyzer-inlay-hints
        # hook window -group rust-inlay-hints InsertIdle .* rust-analyzer-inlay-hints
        # hook -once -always window WinSetOption filetype=.* %{
        #   remove-hooks window rust-inlay-hints
        # }

        # hook window -group semantic-tokens BufReload .* lsp-semantic-tokens
        # hook window -group semantic-tokens NormalIdle .* lsp-semantic-tokens
        # hook window -group semantic-tokens InsertIdle .* lsp-semantic-tokens
        # hook -once -always window WinSetOption filetype=.* %{
        #   remove-hooks window semantic-tokens
        # }
      }

      hook global WinSetOption filetype=ruby %{
        expandtab
        set-option buffer softtabstop 2
        set-option buffer tabstop 2
        set-option buffer indentwidth 2

        map buffer normal <a-minus> ': outline-jump-ruby<ret>'
        map buffer goto d '<a-i>w: outline-jump-ruby %val{selection}<ret>' -docstring 'Def (same file)'
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

      hook global WinSetOption filetype=idris %{
        expandtab
        set-option buffer softtabstop 2
        set-option buffer tabstop 2
        set-option buffer indentwidth 2

        set buffer idris_node_binary_path "${pkgs.nodejs}/bin/node"
        map global user d ': enter-user-mode idris-ide<ret>' -docstring 'Idris IDE'
      }
    '';
  };

  # plugins
  home.file.".config/kak/colors".source =
    "${kakoune.mkColors colors}/share/kak/colors";
  home.file.".config/kak/autoload".source =
    "${kakoune.mkPlugins plugins}/share/kak/autoload";

  home.file.".config/kak-lsp/kak-lsp.toml".text = ''
    [language.rust]
    filetypes = ["rust"]
    roots = ["Cargo.toml"]
    command = "${pkgs.rust-analyzer}/bin/rust-analyzer"
  '';
}
