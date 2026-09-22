{ pkgs, ... }: {
  programs.helix = {
    enable = true;
    defaultEditor = true;

    settings = {
      theme = "base16_default";

      editor = {
        soft-wrap.enable = true;

        end-of-line-diagnostics = "hint";
        inline-diagnostics = {
          cursor-line = "warning";
        };
      };

      keys.insert = {
        f.d = "normal_mode";
      };
    };

    languages = {
      language = [
        {
          name = "nix";
          auto-format = true;
          formatter.command = "${pkgs.nixfmt-tree}/bin/nixfmt-tree";
        }
      ];
    };
  };
}
