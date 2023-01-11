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
    ];

    settings = {
      theme = "everforest_dark";

      editor = {
        lsp.display-messages = true;

        file-picker.hidden = false;
      };

      keys = {
        insert.f.d = "normal_mode";
        normal."#" = ":reflow 80";
      };
    };
  };
}
