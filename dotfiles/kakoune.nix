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
  plugins = (lib.mapAttrsToList (_: plugin: plugin) pluginAttrs) ++ [ ];

  colorAttrs = lib.mapAttrs (name: source:
    kakoune.mkColorPlugin {
      name = name;
      src = source;
    }) colorSources;
  colors = lib.mapAttrsToList (_: color: color) colorAttrs;

  kak-lsp = import ../pkgs/kak-lsp { };
in {
  home.packages =
    [ pkgs.shellcheck (pkgs.callPackages ../pkgs/kak-session { }) ];

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
          commands = "mkdir %val{bufname}";
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

        # Haskell
        {
          name = "WinCreate";
          option = ".*.hs";
          commands = ''
            evaluate-commands %sh{
              if which ormolu > /dev/null; then
                echo 'set-option buffer formatcmd ormolu'
              fi
            }
          '';
        }
        {
          commands = "format";
          name = "BufWritePre";
          option = ".*.hs";
        }

        # Python
        {
          name = "WinCreate";
          option = ".*.py";
          commands = ''
            evaluate-commands %sh{
              if which black > /dev/null; then
                echo 'set-option buffer formatcmd "black - --quiet --fast"'
              fi
            }
          '';
        }
        {
          commands = "format";
          name = "BufWritePre";
          option = ".*.py";
        }

        # JavaScript
        {
          name = "WinCreate";
          option = ".*.js";
          commands = ''
            evaluate-commands %sh{
              if which prettier > /dev/null; then
                echo 'set-option buffer formatcmd "prettier --parser=typescript"'
              fi
            }
          '';
        }
        {
          commands = "format";
          name = "BufWritePre";
          option = ".*.js";
        }

        # Indents
        {
          name = "WinCreate";
          option = ".*.(nix|rb|hs)";
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

        # kakboard
        {
          name = "WinCreate";
          option = ".*";
          commands = "kakboard-enable";
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

      # LSP
      eval %sh{${kak-lsp}/bin/kak-lsp --kakoune -s $kak_session}
      map global user l ': enter-user-mode lsp<ret>' -docstring 'LSP'
      lsp-enable
      lsp-auto-hover-enable
    '';
  };

  # plugins
  home.file.".config/kak/colors".source =
    "${kakoune.mkColors colors}/share/kak/colors";
  home.file.".config/kak/autoload".source =
    "${kakoune.mkPlugins plugins}/share/kak/autoload";
  home.file.".config/kak/kak-tree.toml".text = "";
  home.file."Library/Preferences/kak-lsp/kak-lsp.toml".text = ''
    snippet_support = true
    verbosity = 2

    [semantic_scopes]
    # Map textmate scopes to kakoune faces for semantic highlighting
    # the underscores are translated to dots, and indicate nesting.
    # That is, if variable_other_field is omitted, it will try the face for
    # variable_other and then variable
    #
    # To see a list of available scopes in the debug buffer, run lsp-semantic-available-scopes
    variable="variable"
    entity_name_function="function"
    entity_name_type="type"
    variable_other_enummember="variable"
    entity_name_namespace="module"

    [server]
    # exit session if no requests were received during given period in seconds
    # works only in unix sockets mode (-s/--session)
    # set to 0 to disable
    timeout = 0

    [language.ruby]
    filetypes = ["ruby"]
    roots = ["Gemfile"]
    command = "solargraph"
    args = ["stdio"]

    # [language.haskell]
    # filetypes = ["haskell"]
    # roots = ["Setup.hs", "stack.yaml", "*.cabal"]
    # command = "hie"
    # args = ["--lsp"]

    [language.elm]
    filetypes = ["elm"]
    roots = ["elm.json"]
    command = "${pkgs.elmPackages.elm-language-server}/bin/elm-language-server"
    args = ["--stdio"]

    [language.elm.initialization_options]
    runtime = "node"
    elmPath = "elm"
    elmFormatPath = "elm-format"
    elmTestPath = "elm-test"
    elmAnalyseTrigger = "change"
  '';
}
