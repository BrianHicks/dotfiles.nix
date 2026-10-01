{ pkgs, ... }: {
  programs.helix = {
    enable = true;
    defaultEditor = true;

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
        pylsp =
          let
            # primary project is still on Python 3.13. Just use that for the
            # sake of consistency.
            pylspEnv = pkgs.python313.withPackages (ps: [
              ps.python-lsp-server
              ps.pylsp-mypy
            ]);

            # Use project mypy and plugins first, then the global one.
            mypyWrapper = pkgs.writeShellScript "mypy-prefer-venv" ''
              if [ -n "$VIRTUAL_ENV" ] && [ -x "$VIRTUAL_ENV/bin/mypy" ]; then
                exec "$VIRTUAL_ENV/bin/mypy" "$@"
              fi

              for arg in "$@"; do
                case "$arg" in
                  -*) ;;
                  *) target="$arg" ;;
                esac
              done

              dir=$(dirname "''${target:-$PWD}")
              while [ "$dir" != "/" ]; do
                if [ -x "$dir/.venv/bin/mypy" ]; then
                  exec "$dir/.venv/bin/mypy" "$@"
                fi
                dir=$(dirname "$dir")
              done

              exec "${pylspEnv}/bin/mypy" "$@"
            '';
          in
          {
            command = "${pylspEnv}/bin/pylsp";

            # required for pylsp-mypy to honor mypy_command. Note that this
            # also lets a project's pyproject.toml set mypy_command, so opening
            # an untrusted repo can run its code.
            environment.PYLSP_MYPY_ALLOW_DANGEROUS_CODE_EXECUTION = "1";

            config.pylsp.plugins.pylsp_mypy = {
              enabled = true;
              live_mode = true;
              mypy_command = [ "${mypyWrapper}" ];
            };
          };

        ruff = {
          command = "${pkgs.ruff}/bin/ruff";
          args = [ "server" ];
        };

        clojure-lsp.command = "${pkgs.clojure-lsp}/bin/clojure-lsp";
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
          # ruff for linting + formatting, pylsp only for mypy
          language-servers = [
            "pylsp"
            "ruff"
          ];
        }
      ];
    };
  };
}
