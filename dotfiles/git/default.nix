{ pkgs, ... }:
{
  home.packages = [ pkgs.git-gclone ];

  programs.git = {
    enable = true;

    settings = {
      user = {
        name = "Brian Hicks";
        email = "brian@brianthicks.com";
      };

      alias = {
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

    ignores = [
      ".direnv"
      ".DS_Store"
    ];

    signing = {
      key = null;
      # signByDefault = true;

      # The gpg2 binary provided by Nix doesn't work with the gpg paths
      # provided by gpg-tools for Mac. Use the thing that's just on the PATH,
      # although I'm not terribly happy with that.
      signer = "gpg";
    };
  };
}
