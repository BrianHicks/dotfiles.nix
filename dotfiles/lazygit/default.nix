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

        diffRenderers = [
          {
            colorArg = "always";
            command = "${pkgs.difftastic}/bin/difft --color=always";
            type = "extDiff";
          }
        ];
      };
    };
  };

  home.shellAliases = {
    lg = "lazygit";
  };
}
