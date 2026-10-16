{ pkgs, lib, ... }: {
  programs.helix = {
    enable = true;
    defaultEditor = true;

    package = pkgs.symlinkJoin {
      name = "helix-venv";
      paths = [ pkgs.helix ];
      nativeBuildInputs = [ pkgs.makeWrapper ];
      postBuild = ''
        wrapProgram $out/bin/hx --run '
          # source venv automatically first if available to get language servers
          if [ -z "$VIRTUAL_ENV" ]; then
            for arg in "$@"; do
              case "$arg" in
                -*) ;;
                *) target="$arg"; break ;;
              esac
            done
            if [ -d "''${target:-$PWD}" ]; then
              dir=$(cd "''${target:-$PWD}" && pwd)
            else
              dir=$(cd "$(dirname "$target")" 2>/dev/null && pwd || pwd)
            fi
            while [ "$dir" != "/" ]; do
              if [ -f "$dir/.venv/pyvenv.cfg" ]; then
                export VIRTUAL_ENV="$dir/.venv"
                export PATH="$VIRTUAL_ENV/bin:$PATH"
                break
              fi
              dir=$(dirname "$dir")
            done
          fi
        '
      '';
    };

    settings = {
      theme = "adwaita-dark";

      editor = {
        soft-wrap.enable = true;

        end-of-line-diagnostics = "hint";
        inline-diagnostics = {
          cursor-line = "warning";
        };

        cursor-shape = {
          insert = "bar";
          normal = "block";
          select = "underline";
        };

        # need .github, .rfcs, etc
        file-picker.hidden = false;
      };

      keys.insert = {
        f.d = "normal_mode";
      };
    };

    languages = {
      # defaults at https://github.com/helix-editor/helix/blob/master/languages.toml
      language-server = {
        pylsp.config.pylsp.plugins.pylsp_mypy = {
          enabled = true;
          live_mode = true;
        };

        ruff.command = lib.getExe pkgs.ruff;

        clojure-lsp.command = lib.getExe pkgs.clojure-lsp;

        typescript-language-server.command = lib.getExe pkgs.typescript-language-server;

        vscode-css-language-server.command = lib.getExe pkgs.vscode-css-languageserver;
      };

      language = [
        {
          name = "nix";
          auto-format = true;
          formatter.command = "${pkgs.nixfmt-tree}/bin/nixfmt-tree";
        }
        {
          name = "python";
          auto-format = true;
          language-servers = [
            {
              name = "pylsp";
              except-features = [ "format" ];
            }
            "ruff"
          ];
        }
        {
          name = "clojure";
          auto-format = true;
        }
      ];
    };
  };
}
