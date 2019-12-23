{ pkgs, lib, ... }:
let
  sources = import ../nix/sources.nix;
  nixpkgs = import sources.nixpkgs { };
  kakoune = import ../lib/kakoune.nix {
    pkgs = nixpkgs.pkgs;
    lib = nixpkgs.lib;
  };

  # plugins
  pluginSources = lib.filterAttrs (_: source: lib.attrByPath [ "kakoune" ] "" source == "plugin") sources;
  colorSources = lib.filterAttrs (_: source: lib.attrByPath [ "kakoune" ] "" source == "colors") sources;

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
          commands = ''
            declare-user-mode surround
            map global surround s ':surround<ret>' -docstring 'Surround'
            map global surround c ':change-surround<ret>' -docstring 'Change'
            map global surround d ':delete-surround<ret>' -docstring 'Delete'
            map global surround t ':select-surrounding-tag<ret>' -docstring 'Select tag'
            map global user s ':enter-user-mode surround<ret>' -docstring 'Surround'
          '';
          name = "WinCreate";
          option = ".*";
        }
        {
          commands = "mkdir-buffer";
          name = "BufWritePre";
          option = ".*";
        }
      ];

      keyMappings = [
        {
          mode = "normal";
          key = "<c-t>";
          effect = ": fzf-mode<ret>";
        }
      ];
    };
  };

  # plugins
  home.file.".config/kak/colors".source = "${kakoune.mkColors colors}/share/kak/colors";
  home.file.".config/kak/autoload".source = "${kakoune.mkPlugins plugins}/share/kak/autoload";
}
