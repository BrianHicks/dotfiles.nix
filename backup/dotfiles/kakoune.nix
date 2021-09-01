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

      # language server
      # eval %sh{${kak-lsp}/bin/kak-lsp --config ~/.config/kak-lsp/kak-lsp.toml --kakoune -s $kak_session}
      # map global user l ': enter-user-mode lsp<ret>' -docstring 'LSP'
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
