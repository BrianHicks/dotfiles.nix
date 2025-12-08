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
    };
  };
}
