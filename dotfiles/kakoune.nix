{ pkgs, lib, ... }:
let
  sources = import ../nix/sources.nix;
  nixpkgs = import sources.nixpkgs { };
  kakoune = import ../lib/kakoune.nix {
    pkgs = nixpkgs.pkgs;
    lib = nixpkgs.lib;
  };

  # plugins
  pluginSources = lib.filterAttrs
    (_: source: lib.attrByPath [ "kakoune" ] "" source == "plugin") sources;
  colorSources = lib.filterAttrs
    (_: source: lib.attrByPath [ "kakoune" ] "" source == "colors") sources;

  kak-lsp = import ../pkgs/kak-lsp { };

  pluginAttrs = lib.mapAttrs (name: source:
    kakoune.mkPlugin {
      name = name;
      src = source;
    }) pluginSources;
  plugins = lib.mapAttrsToList (_: plugin: plugin) pluginAttrs;

  colorAttrs = lib.mapAttrs (name: source:
    kakoune.mkColorPlugin {
      name = name;
      src = source;
    }) colorSources;
  colors = lib.mapAttrsToList (_: color: color) colorAttrs;
in {
  home.packages = [ kak-lsp pkgs.shellcheck ];

  programs.kakoune = {
    enable = true;
    config = {
      colorScheme = "palenight";
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
          commands = "set-option buffer formatcmd nixfmt";
          name = "WinCreate";
          option = ".*.nix";
        }
        {
          commands = "format";
          name = "BufWritePre";
          option = ".*.nix";
        }
      ];

      keyMappings = [
        {
          mode = "normal";
          key = "<c-t>";
          effect = ": fzf-mode<ret>";
        }
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

        # kak-lsp
        {
          mode = "goto";
          key = "r";
          effect = "<esc>: lsp-find-error<ret>";
          docstring = "next error";
        }
        {
          mode = "goto";
          key = "<a-r>";
          effect = "<esc>: lsp-find-error --previous<ret>";
          docstring = "previous error";
        }
        {
          mode = "user";
          key = "l";
          effect = ": enter-user-mode lsp<ret>";
          docstring = "language server";
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

      # kak-lsp
      eval %sh{kak-lsp --kakoune --session $kak_session}
      lsp-enable
    '';
  };

  # plugins
  home.file.".config/kak/colors".source =
    "${kakoune.mkColors colors}/share/kak/colors";
  home.file.".config/kak/autoload".source =
    "${kakoune.mkPlugins plugins}/share/kak/autoload";

  # kak-lsp
  home.file."Library/Preferences/kak-lsp/kak-lsp.toml".text = ''
    snippet_support = false
    verbosity = 2

    [server]
    # exit session if no requests were received during given period in seconds
    # works only in unix sockets mode (-s/--session)
    # set to 0 to disable
    timeout = 1800 # seconds = 30 minutes

    [language.haskell]
    filetypes = ["haskell"]
    roots = ["Setup.hs", "stack.yaml", "*.cabal"]
    command = "hie"
    args = ["--lsp"]

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
    elmAnalyseTrigger = "never"
  '';
}
