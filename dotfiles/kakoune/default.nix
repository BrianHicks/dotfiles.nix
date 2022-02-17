{ pkgs, ... }:
let
  copyCommand =
    if pkgs.stdenv.isDarwin then "pbcopy" else "${pkgs.xclip}/bin/xclip -in";
  pasteCommand =
    if pkgs.stdenv.isDarwin then "pbpaste" else "${pkgs.xclip}/bin/xclip -out";

  similar-sort-files-cmd = arg:
    "git ls-files --others --cached --exclude-standard | ${pkgs.similar-sort}/bin/similar-sort ${arg} | grep -v ${arg} | fzf --tiebreak index";
in {
  home.packages = [
    # there's no configuration option for shellcheck.kak; it has to be in the PATH
    # to work!
    pkgs.shellcheck
  ];

  programs.kakoune = {
    enable = true;

    plugins = with pkgs.kakounePlugins; [
      active-window
      auto-pairs
      kak-ayu
      kak-tmux-command
      kak-tree
      kakoune-auto-percent
      kakoune-find
      kakoune-idris
      kakoune-surround
      prelude-kak
      shellcheck-kak
      smarttab-kak
      tug

      (pkgs.kakouneUtils.buildKakounePlugin {
        name = "dotfiles";
        src = ./rc;
      })
    ];

    config = {
      colorScheme = "ayu-mirage";
      scrollOff.lines = 5;
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
      # hide changelog on startup
      set global startup_info_version 20211107

      # escape with fd
      hook global InsertChar d %{ try %{
        exec -draft hH <a-k>fd<ret> d
        exec <esc>
      }}

      # exit from insert mode and save. Useful for triggering file watchers
      # or whatever, as the alternative is `fd:w<ret>`
      map global insert <c-o> '<esc>: write<ret>'
      map global user , ': write<ret>' -docstring "Save current file"

      # automatically create directories on save
      # hook global BufWritePre .* %{ mkdir %val{bufname} }

      # nicer window splits
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

      # clipboard handling
      # inspired by https://github.com/mawww/config/blob/43bd5cea453d629dd119d361cb237d433d09a0eb/kakrc#L61-L75
      map global user -docstring 'paste (after) from clipboard' P '!${pasteCommand}<ret>'
      map global user -docstring 'paste (before) from clipboard' p '<a-!>${pasteCommand}<ret>'
      map global user -docstring 'yank to clipboard' y '<a-|>${copyCommand}<ret>: echo -markup %{{Information}copied selection to clipboard}<ret>'
      map global user -docstring 'replace from clipboard' R '|${pasteCommand}<ret>'

      # selections
      map global user Z '<a-z>aZ' -docstring 'Add to selection'
      map global user a 's[^, ]+<ret>' -docstring 'Split selection into arguments'

      # surrounding
      declare-user-mode surround
      map global surround s ': surround<ret>' -docstring 'Surround'
      map global surround c ': change-surround<ret>' -docstring 'Change'
      map global surround d ': delete-surround<ret>' -docstring 'Delete'
      map global surround t ': select-surrounding-tag<ret>' -docstring 'Select tag'
      map global user s ': enter-user-mode surround<ret>' -docstring 'Surround'

      # kak-subvert
      declare-user-mode subvert
      map global subvert c '<esc> | ${pkgs.kak-subvert}/bin/kak-subvert camel <ret>' -docstring 'convert to camelCase'
      map global subvert k '<esc> | ${pkgs.kak-subvert}/bin/kak-subvert kebab <ret>' -docstring 'convert to kebab-case'
      map global subvert p '<esc> | ${pkgs.kak-subvert}/bin/kak-subvert pascal <ret>' -docstring 'convert to PascalCase'
      map global subvert . '<esc> | ${pkgs.kak-subvert}/bin/kak-subvert sentence <ret>' -docstring 'convert to Sentence case'
      map global subvert s '<esc> | ${pkgs.kak-subvert}/bin/kak-subvert snake <ret>' -docstring 'convert to snake_case'
      map global subvert S '<esc> | ${pkgs.kak-subvert}/bin/kak-subvert screaming <ret>' -docstring 'convert to SCREAMING_CASE'
      map global subvert t '<esc> | ${pkgs.kak-subvert}/bin/kak-subvert train <ret>' -docstring 'convert to Train-Case'
      map global subvert T '<esc> | ${pkgs.kak-subvert}/bin/kak-subvert title <ret>' -docstring 'convert to Title Case'
      map global subvert u '<esc> | ${pkgs.kak-subvert}/bin/kak-subvert ugly <ret>' -docstring 'convert to Ugly_Case'

      map global user c ': enter-user-mode subvert<ret>' -docstring 'case changes'

      # File Browsing
      require-module open
      set global similar_sort_path '${pkgs.similar-sort}/bin/similar-sort'
      map global normal <minus> ': open-similar<ret>'
      map global normal _ ': open-similar-buffer<ret>'

      # kak-tree
      set global tree_cmd '${pkgs.kak-tree}/bin/kak-tree'

      declare-user-mode tree
      map global user t ': enter-user-mode -lock tree<ret>' -docstring 'Tree Selection'
      map global tree h ': tree-select-parent-node<ret>' -docstring 'Parent'
      map global tree <a-l> ': tree-select-children<ret>' -docstring 'Children'
      map global tree l ': tree-select-first-child<ret>' -docstring 'First Child'
      map global tree j ': tree-select-next-node<ret>' -docstring 'Next Node'
      map global tree k ': tree-select-previous-node<ret>' -docstring 'Previous Node'
      map global tree d ': tree-select-parent-node value_declaration<ret>' -docstring 'Parent Declaration'

      map global user r ': tree-select-parent-node<ret>' -docstring 'Select Parent'

      # sending commands places
      declare-user-mode tmux-command
      map global user x ': tmux-send-command<ret>' -docstring 'Execute command'
      map global user X ': enter-user-mode tmux-command<ret>' -docstring 'Set up command'
      map global tmux-command : ':tmux-send-command ' -docstring 'Send a one-off command'
      map global tmux-command c ':tmux-set-command ' -docstring 'Set the command'
      map global tmux-command t ':tmux-set-target ' -docstring 'Set the target'

      # outline jumping
      set global tree_grepper_path "${pkgs.tree-grepper}/bin/tree-grepper"
      set global tree_grepper_fzf_path "${pkgs.fzf}/bin/fzf"

      # language server
      # eval %sh{${pkgs.kak-lsp}/bin/kak-lsp --config ~/.config/kak-lsp/kak-lsp.toml --kakoune -s $kak_session}
      # map global user l ': enter-user-mode lsp<ret>' -docstring 'LSP'
      # set global lsp_cmd "${pkgs.kak-lsp}/bin/kak-lsp -s %val{session} -vvv --log kak-lsp.log --config ~/.config/kak-lsp/kak-lsp.toml"
      # set global lsp_hover_anchor true

      # define-command lsp-enable-window-without-completion %{
      #   # enable LSP for the window, but then...
      #   lsp-enable-window

      #   # ... remove the hooks that it installs and ...
      #   remove-hooks window lsp

      #   # ... re-install only the ones I want
      #   hook -group lsp window WinClose .* lsp-did-close
      #   hook -group lsp window BufWritePost .* lsp-did-save
      #   hook -group lsp window WinSetOption lsp_config=.* lsp-did-change-config
      #   hook -group lsp window WinSetOption lsp_server_configuration=.* lsp-did-change-config
      # 	# this InsertIdle hook is basically the one I want to try going without.
      # 	# It means I won't get completions, but I think that's OK! I notice a lot
      # 	# of lag while using kak-lsp, especially in rust-analyzer, and I don't
      # 	# need lints as I'm typing.
      #   # hook -group lsp window InsertIdle .* lsp-completion
      #   hook -group lsp window NormalIdle .* %{
      #     lsp-did-change
      #     %sh{if $kak_opt_lsp_auto_highlight_references; then echo "lsp-highlight-references"; else echo "nop"; fi}
      #   }
      # }

      # Languages
      define-command expandtab-with-width -params 1 -hidden %{
        expandtab
        set-option buffer tabstop %arg{1}
        set-option buffer softtabstop %arg{1}
        set-option buffer indentwidth %arg{1}
      }

      hook global WinSetOption filetype=cue %{
        noexpandtab
        set-option buffer tabstop 4

        # formatting
        set-option buffer formatcmd "${pkgs.cue}/bin/cue fmt -s -"
        hook buffer BufWritePre .* format
      }

      hook global WinSetOption filetype=elm %{
        expandtab-with-width 4

        # formatting
        set-option buffer formatcmd 'elm-format --stdin'
        hook buffer BufWritePre .* format

        # extra commands
        map buffer user i ': elm-copy-import-line<ret>' -docstring 'Copy an import line'
        map buffer user d ': execute-keys -draft y,ss)mliDebug.log<space>"<esc>Pi"<space><esc>' -docstring 'Debug selection'

        map buffer normal <a-minus> ': outline-jump-elm<ret>'
      }

      hook global WinSetOption filetype=haskell %{
        expandtab-with-width 2

        evaluate-commands %sh{
          if which ormolu > /dev/null; then
            echo 'set-option buffer formatcmd ormolu'
            echo 'hook buffer BufWritePre .* format'
          fi
        }

        map buffer normal <a-minus> ': outline-jump-haskell<ret>'
      }

      hook global WinSetOption filetype=html %{
        expandtab-with-width 2
      }

      hook global WinSetOption filetype=idris %{
        expandtab-with-width 2

        set buffer idris_node_binary_path "${pkgs.nodejs}/bin/node"
        map global user d ': enter-user-mode idris-ide<ret>' -docstring 'Idris IDE'
      }

      hook global WinSetOption filetype=json %{
        expandtab-with-width 2
      }

      hook global WinSetOption filetype=javascript %{
        expandtab-with-width 2

        evaluate-commands %sh{
          if which prettier > /dev/null; then
            echo 'set-option buffer formatcmd "prettier --parser=typescript"'
            echo 'hook buffer BufWritePre .* format'
          fi
        }
      }

      hook global WinSetOption filetype=nix %{
        expandtab-with-width 2

        # formatting
        set-option buffer formatcmd ${pkgs.nixfmt}/bin/nixfmt
        hook buffer BufWritePre .* format
      }

      hook global WinSetOption filetype=python %{
        expandtab-with-width 4

        evaluate-commands %sh{
          if which black > /dev/null; then
            echo 'set-option buffer formatcmd "black - --quiet --fast"'
            echo 'hook buffer BufWritePre .* format'
          fi
        }
      }

      hook global BufCreate .*[.](roc) %{
        set-option buffer filetype coffee
      }

      hook global WinSetOption filetype=ruby %{
        expandtab-with-width 2

        map buffer normal <a-minus> ': outline-jump-ruby<ret>'
      }

      hook global WinSetOption filetype=rust %{
        expandtab-with-width 4

        map buffer normal <a-minus> ': outline-jump-rust<ret>'

        evaluate-commands %sh{
          if which rustfmt > /dev/null; then
            echo 'set-option buffer formatcmd "rustfmt --emit stdout"'
            echo 'hook buffer BufWritePre .* format'
          fi
        }
      }

      hook global WinSetOption filetype=sh %{
        expandtab-with-width 2

        hook buffer BufWritePre .* lint
      }

      hook global WinSetOption filetype=terraform %{
        expandtab-with-width 2
        
        evaluate-commands %sh{
          if which terraform > /dev/null; then
            echo 'set-option buffer formatcmd "terraform fmt -"'
            echo 'hook buffer BufWritePre .* format'
          fi
        }
      }

      hook global WinSetOption filetype=ts %{
        expandtab-with-width 2
      }

      hook global WinSetOption filetype=yaml %{
        expandtab-with-width 2
      }

      hook global WinSetOption filetype=dhall %{
        expandtab-with-width 2

        # lsp-enable-window
        set-option buffer formatcmd "dhall format"
      }
    '';
  };

  home.file.".config/kak-lsp/kak-lsp.toml".text = ''
    [language.dhall]
    filetypes = ["dhall"]
    roots = []
    command = "${pkgs.dhall-lsp-server}/bin/dhall-lsp-server"

    [language.rust]
    filetypes = ["rust"]
    roots = ["Cargo.toml"]
    command = "${pkgs.rust-analyzer}/bin/rust-analyzer"

    # Semantic tokens support
    # See https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_semanticTokens
    # for the default list of tokens and modifiers.
    # However, many language servers implement their own values.
    # Make sure to check the output of `lsp-capabilities` and each server's documentation and source code as well.
    # Examples:
    # - TypeScript: https://github.com/microsoft/vscode-languageserver-node/blob/2645fb54ea1e764aff71dee0ecc8aceff3aabf56/client/src/common/semanticTokens.ts#L58
    # - Rust Analyzer: https://github.com/rust-analyzer/rust-analyzer/blob/f6da603c7fe56c19a275dc7bab1f30fe1ad39707/crates/ide/src/syntax_highlighting.rs#L42
    [[semantic_tokens]]
    token = "comment"
    face = "documentation"
    modifiers = ["documentation"]

    [[semantic_tokens]]
    token = "comment"
    face = "comment"

    [[semantic_tokens]]
    token = "function"
    face = "function"

    [[semantic_tokens]]
    token = "keyword"
    face = "keyword"

    [[semantic_tokens]]
    token = "namespace"
    face = "module"

    [[semantic_tokens]]
    token = "operator"
    face = "operator"

    [[semantic_tokens]]
    token = "string"
    face = "string"

    [[semantic_tokens]]
    token = "type"
    face = "type"

    [[semantic_tokens]]
    token = "variable"
    face = "default+d"
    modifiers = ["readonly"]

    [[semantic_tokens]]
    token = "variable"
    face = "default+d"
    modifiers = ["constant"]

    [[semantic_tokens]]
    token = "variable"
    face = "variable"
  '';
}
