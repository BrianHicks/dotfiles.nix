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
  home.packages = [ pkgs.shellcheck ];

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

      # outline jumping
      set global tree_grepper_path "${tree-grepper}/bin/tree-grepper"
      set global tree_grepper_fzf_path "${pkgs.fzf}/bin/fzf"

      # language server
      # eval %sh{${kak-lsp}/bin/kak-lsp --config ~/.config/kak-lsp/kak-lsp.toml --kakoune -s $kak_session}
      # map global user l ': enter-user-mode lsp<ret>' -docstring 'LSP'

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

  home.file.".config/kak-lsp/kak-lsp.toml".text = ''
    [language.rust]
    filetypes = ["rust"]
    roots = ["Cargo.toml"]
    command = "${pkgs.rust-analyzer}/bin/rust-analyzer"
  '';
}
