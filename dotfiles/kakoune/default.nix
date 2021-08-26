{ pkgs, ... }:
let
  copyCommand = if pkgs.stdenv.isDarwin then "pbcopy" else "UNKNOWN";
  pasteCommand = if pkgs.stdenv.isDarwin then "pbpaste" else "UNKNOWN";
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
    '';
  };
}
