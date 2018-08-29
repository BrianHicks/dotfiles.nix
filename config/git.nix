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
      gclone = "!\"gclone() { mkdir -p ~/code/$1; git clone git@github.com:$1.git ~/code/$1; }; gclone\"";
      publish = "!git push -u origin $(git rev-parse --abbrev-ref HEAD)";
      st = "status";
    };

    extraConfig = {
      rerere.enabled = true;

      "filter \"lfs\"" = {
        clean = "git-lfs clean -- %f";
        smudge = "git-lfs smudge -- %f";
        process = "git-lfs filter-process";
        required = true;
      };

      # magit
      github.user = "BrianHicks";
    };

    ignores = [
      "*.swp"
      "*~"
      ".#*"
      ".DS_Store"
      ".python-version"
      ".ruby-version"
      ".vagrant"
    ];
  };

  home.packages = with pkgs; [ git-lfs ];
}
