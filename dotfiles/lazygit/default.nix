{ pkgs, ... }: {
  programs.lazygit = {
    enable = true;
    enableZshIntegration = true;

    settings = {
      reporting = "off";

      update.method = "never"; # managed through nixpkgs

      keybindings = {
        universal.return = "q";
        universal.createRebaseOptionsMenu = "M";
        branches.mergeIntoCurrentBranch = "m";
      };

      git.pagers = [{
        colorArg = "always";
        externalDiffCommand = "${pkgs.difftastic}/bin/difft --color=always";
      }];

      git.overrideGpg = true; # prevents spawning a separate process on commits

      # my SSH agent (1password) currently spams auth prompts when using lazygit, so
      # I'm turning off auto-fetch stuff, hopefully temporarily!
      git.autoFetch = false;
      git.autoRefresh = false;
    };
  };
}
