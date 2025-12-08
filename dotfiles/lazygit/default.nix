{ pkgs, ... }:
{
  programs.lazygit = {
    enable = true;
    enableZshIntegration = true;

    settings = {
      reporting = "off";

      update.method = "never"; # managed through nixpkgs

      keybindings = {
        universal.createRebaseOptionsMenu = "M";
        branches.mergeIntoCurrentBranch = "m";
      };

      git = {
        overrideGpg = true;

        # Triggers 1PW auth prompts constantly; stop it!
        autoFetch = false;
        autoRefresh = false;

        pagers = [
          {
            colorArg = "always";
            externalDiffCommand = "${pkgs.difftastic}/bin/difft --color=always";
          }
        ];
      };
    };
  };
}
