{ pkgs, ... }:
let
  lazygit-config = if pkgs.stdenv.isDarwin then
    "Library/Application Support/jesseduffield/lazygit/config.yml"
  else
    ".config/jesseduffield/lazygit/config.yml";
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

  programs.gh.enable = true;

  home.file."${lazygit-config}".text = builtins.toJSON {
    reporting = "off";
    startupPopupVersion = 1;

    update.method = "never"; # managed through nixpkgs

    keybindings = {
      universal.return = "q";
      universal.createRebaseOptionsMenu = "M";
      branches.mergeIntoCurrentBranch = "m";
    };

    gui.theme = {
      lightTheme = false;
      activeBorderColor = [ "green" "bold" ];
      inactiveBorderColor = [ "white" ];
      optionsTextColor = [ "blue" ];
      selectedLineBgColor = [ "bold" ];
    };

    git.paging = {
      colorArg = "always";
      pager = "${pkgs.gitAndTools.delta}/bin/delta --paging=never";
    };
  };

  home.packages =
    [ pkgs.git-lfs pkgs.lazygit pkgs.gitAndTools.delta pkgs.git-gclone ];
}
