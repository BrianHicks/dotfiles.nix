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
        language-server = {
          command = "srb";
          args = [ "typecheck" "--lsp" ];
        };
      }
    ];

    settings = {
      theme = "monokai_pro_machine";

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
