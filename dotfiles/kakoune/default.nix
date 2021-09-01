{ pkgs, ... }:
let
  copyCommand =
    if pkgs.stdenv.isDarwin then "pbcopy" else "${pkgs.xclip}/bin/xclip -in";
  pasteCommand =
    if pkgs.stdenv.isDarwin then "pbpaste" else "${pkgs.xclip}/bin/xclip -out";
in {
  programs.kakoune = {
    enable = true;

    plugins = with pkgs.kakounePlugins; [
      active-window
      kak-auto-pairs
      kak-tree
      kakoune-auto-percent
      kakoune-find
      kakoune-idris
      kakoune-surround
      kak-subvert
      shellcheck-kak
      smarttab-kak
      tug

      (pkgs.kakouneUtils.buildKakounePlugin {
        name = "dotfiles";
        src = ./rc;
      })
    ];

    config = {
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

      # Languages
      define-command expandtab-with-width -params 1 -hidden %{
        expandtab
        set-option buffer tabstop %arg{1}
        set-option buffer softtabstop %arg{1}
        set-option buffer indentwidth %arg{1}
      }

      hook global WinSetOption filetype=elm %{
        expandtab-with-width 4

        # formatting
        set-option buffer formatcmd 'elm-format --stdin'
        hook buffer BufWritePre .* format

        # extra commands
        map buffer user i ': elm-copy-import-line<ret>' -docstring 'Copy an import line'
        map buffer user d ': execute-keys -draft y,ss)mliDebug.log<space>"<esc>Pi"<space><esc>' -docstring 'Debug selection'
      }

      hook global WinSetOption filetype=haskell %{
        expandtab-with-width 2

        evaluate-commands %sh{
          if which ormolu > /dev/null; then
            echo 'set-option buffer formatcmd ormolu'
            echo 'hook buffer BufWritePre .* format'
          fi
        }
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

      hook global WinSetOption filetype=ruby %{
        expandtab-with-width 2
      }

      hook global WinSetOption filetype=rust %{
        expandtab-with-width 4

        evaluate-commands %sh{
          if which rustfmt > /dev/null; then
            echo 'set-option buffer formatcmd rustfmt'
            echo 'hook buffer BufWritePre .* format'
          fi
        }

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
    '';
  };
}
