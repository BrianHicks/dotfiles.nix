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
          # source the nearest venv so language servers (pylsp, mypy, ...) come from the project
          if [ -z "$VIRTUAL_ENV" ]; then
            dir=$PWD
            while [ "$dir" != "/" ]; do
              if [ -f "$dir/.venv/bin/activate" ]; then
                . "$dir/.venv/bin/activate"
                break
              fi
              dir=$(dirname "$dir")
            done
          fi
          if [ -n "$VIRTUAL_ENV" ]; then
            pip install python-lsp-server pylsp-mypy
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
        pylsp.config.pylsp.plugins = {
          pycodestyle.maxLineLength = 5000; # handled by autoformatter
          pylsp_mypy = {
            enabled = true;
            live_mode = true;
          };
        };

        clojure-lsp.command = lib.getExe pkgs.clojure-lsp;

        typescript-language-server.command = lib.getExe pkgs.typescript-language-server;

        vscode-css-language-server.command = lib.getExe pkgs.vscode-css-languageserver;

        codebook = {
          command = "${pkgs.codebook}/bin/codebook-lsp";
          args = [ "serve" ];
        };
      };

      language = [
        {
          name = "nix";
          auto-format = true;
          formatter.command = "${pkgs.nixfmt-tree}/bin/nixfmt-tree";
          language-servers = [
            # built-in config
            "nil"
            "nixd"

            # mine
            "codebook"
          ];
        }
        {
          name = "python";
          auto-format = true;
          language-servers = [
            "pylsp"
            "codebook"
          ];
        }
        {
          name = "clojure";
          auto-format = true;
          language-servers = [
            "clojure-lsp"
            "codebook"
          ];
        }
        {
          name = "rust";
          language-servers = [
            "rust-analyzer"
            "codebook"
          ];
        }
        {
          name = "markdown";
          language-servers = [
            # built-in config
            "marksman"
            "markdown-oxide"
            "rumdl"

            # mine
            "codebook"
          ];
        }
      ];
    };
  };
}
