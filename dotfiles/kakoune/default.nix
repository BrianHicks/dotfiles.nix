{ pkgs, ... }:
let
  copyCommand = if pkgs.stdenv.isDarwin then "pbcopy" else "${pkgs.xclip}/bin/xclip -in";
  pasteCommand = if pkgs.stdenv.isDarwin then "pbpaste" else "${pkgs.xclip}/bin/xclip -out";
in {
  programs.kakoune = {
    enable = true;

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
        exec <exc>
      }}

      # exit from insert mode and save. Useful for triggering file watchers
      # or whatever, as the alternative is `fd:w<ret>`
      map global insert <c-o> '<esc>: write<ret>'
      map global user , ': write<ret>' -docstring "Save current file"

      # automatically create directories on save
      #hook global BufWritePre .* %{ mkdir %val{bufname} }

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

      # Languages
      hook global WinSetOption filetype=nix %{
        #expandtab
        #set-option buffer tabstop 2
        #set-option buffer softtabstop 2
        #set-option buffer indentwidth 2

        # formatting
        set-option buffer formatcmd ${pkgs.nixfmt}/bin/nixfmt
        hook buffer BufWritePre .* format
      }
    '';
  };
}
