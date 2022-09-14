{ pkgs, ... }: {
  programs.helix = {
    enable = true;
    languages = [{
      name = "rust";
      auto-format = true;
    }];

    settings = {
      theme = "monokai_pro_machine";

      editor = {
        lsp.display-messages = true;

        file-picker.hidden = false;
      };

      keys = { insert.f.d = "normal_mode"; };
    };
  };
}
