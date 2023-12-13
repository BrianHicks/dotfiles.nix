{ pkgs, ... }:
let
  lazygit-config =
    if pkgs.stdenv.isDarwin
    then "Library/Application Support/jesseduffield/lazygit/config.yml"
    else ".config/jesseduffield/lazygit/config.yml";
in
{
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
      st = "status";
      yoda = "push --force-with-lease";
      root = "rev-parse --show-toplevel";
    };

    extraConfig = {
      # after upgrading to 21.11 I suddently need to explicitly set an SSH command
      # to connect a port other than :22. How weird! Fortunately it's easy to work
      # around with this but I'm not happy with it. Maybe someday I'll figure this
      # out and come back and remove this line.
      core.sshCommand = "ssh";

      core.fsmonitor = "true";

      init.defaultBranch = "main";

      # make `git push` automatically do `-u origin`
      push.autoSetupRemote = true;

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

    signing = {
      key = null;
      signByDefault = true;

      # The gpg2 binary provided by Nix doesn't work with the gpg paths
      # provided by gpg-tools for Mac. Use the thing that's just on the PATH,
      # although I'm not terribly happy with that.
      gpgPath = "gpg";
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

      # the default highlighted color is a very bright blue that doesn't have
      # sufficient contrast with the foreground.
      selectedLineBgColor = [ "bold" ];
      selectedRangeBgColor = [ "black" "bold" ];
    };

    git.paging = {
      colorArg = "always";
      pager = "${pkgs.gitAndTools.delta}/bin/delta --paging=never";
    };

    git.overrideGpg = true; # prevents spawning a separate process on commits

    # my SSH agent (1password) currently spams auth prompts when using lazygit, so
    # I'm turning off auto-fetch stuff, hopefully temporarily!
    git.autoFetch = false;
    git.autoRefresh = false;
  };

  home.packages = [
    pkgs.gh
    pkgs.git-gclone
    pkgs.git-lfs
    pkgs.gitAndTools.delta
    pkgs.lazygit
  ];
}
