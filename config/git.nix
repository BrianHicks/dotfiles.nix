{ pkgs, ... }:

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
      publish = "!git push -u origin $(git rev-parse --abbrev-ref HEAD)";
      st = "status";
    };

    extraConfig = {
      # REmember REbase REsults
      rerere.enabled = true;

      # LFS
      "filter \"lfs\"".clean = "git-lfs clean -- %f";
      "filter \"lfs\"".smudge = "git-lfs smudge -- %f";
      "filter \"lfs\"".process = "git-lfs filter-process";
      "filter \"lfs\"".required = true;
    };

    # todo: global ignores
  };

  home.packages = with pkgs; [ git-lfs ];
}
