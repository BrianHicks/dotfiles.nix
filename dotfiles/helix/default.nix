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
      {
        name = "elm";
        roots = [ "elm.json" ];

        # I'm hardcoding an elm-language-server binary here because I work
        # on some projects that use a version of nixpkgs that have a too-old
        # version of the language server that doesn't work with Helix.
        language-server.command =
          "${pkgs.elmPackages.elm-language-server}/bin/elm-language-server";
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
