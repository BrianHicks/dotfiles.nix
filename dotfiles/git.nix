{ pkgs, ... }:
let git-gclone = import ../pkgs/git-gclone { };
in {
  programs.git = {
    enable = true;

    userName = "Brian Hicks";
    userEmail = "brian@brianthicks.com";

    aliases = {
      aa = "add --all";
      amend = "commit --amend";
      ci = "commit";
      co = "checkout";
      dc = "diff --cached";
      di = "diff";
      publish = "push -u origin HEAD";
      st = "status";
      yoda = "push --force-with-lease";
      root = "rev-parse --show-toplevel";
    };

    extraConfig = {
      init.defaultBranch = "main";

      rerere.enabled = true;

      "filter \"lfs\"" = {
        clean = "${pkgs.git-lfs}/bin/git-lfs clean -- %f";
        smudge = "${pkgs.git-lfs}/bin/git-lfs smudge -- %f";
        process = "${pkgs.git-lfs}/bin/git-lfs filter-process";
        required = true;
      };
    };

    ignores = [ ".direnv" ".DS_Store" ];

    delta = {
      enable = true;
      options = {
        syntax-theme = "1337";
        plus-color = "#32473d";
        minus-color = "#643632";
        features = "line-numbers";
        whitespace-error-style = "22 reverse";
      };
    };
  };

  programs.gh.enabled = true;

  home.file."Library/Application Support/jesseduffield/lazygit/config.yml".text =
    builtins.toJSON {
      reporting = "off";
      startupPopupVersion = 1;

      update.method = "never"; # managed through nixpkgs

      keybindings = {
        universal.return = "q";
        universal.createRebaseOptionsMenu = "M";
        branches.mergeIntoCurrentBranch = "m";
        commits.moveUpCommit = "<a-j>";
        commits.moveDownCommit = "<a-k>";
      };

      gui.theme = {
        lightTheme = false;
        activeBorderColor = [ "green" "bold" ];
        inactiveBorderColor = [ "white" ];
        optionsTextColor = [ "blue" ];
        selectedLineBgColor = [ "bold" ];
      };
    };

  home.packages = with pkgs; [
    git-lfs
    lazygit
    pkgs.gitAndTools.delta
    git-gclone
  ];
}
