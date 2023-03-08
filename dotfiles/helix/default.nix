{ pkgs, ... }: {
  programs.helix = {
    enable = true;
    languages = [
      {
        name = "rust";
        auto-format = true;
      }
      {
        name = "ruby";
        roots = [ "Gemfile" "Gemfile.lock" ];
        file-types = [
          # from upstream
          "rb"
          "rake"
          "rakefile"
          "irb"
          "gemfile"
          "gemspec"
          "Rakefile"
          "Gemfile"
          "rabl"
          "jbuilder"
          "jb"

          # what I added
          "rbi"
        ];

        language-server = {
          command = "srb";
          args = [ "typecheck" "--lsp" "--enable-all-beta-lsp-features" ];
        };
      }
      {
        name = "elm";
        language-server = {
          command = "${pkgs.elmPackages.elm-language-server}/bin/elm-language-server";
        };
      }
    ];

    settings = {
      theme = "everforest_dark";

      editor = {
        lsp.display-messages = true;

        file-picker.hidden = false;

        color-modes = true;

        indent-guides.render = true;

        cursor-shape = {
          insert = "bar";
          normal = "block";
          select = "underline";
        };
      };

      keys = {
        insert.f.d = "normal_mode";

        normal = {
          "#" = ":reflow 80";
          "{" = "goto_prev_change";
          "}" = "goto_next_change";
        };
      };
    };
  };
}
