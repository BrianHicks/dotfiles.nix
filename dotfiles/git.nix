{ pkgs, ... }:

{
  programs.git = {
    enable = true;

    userName = "Brian Hicks";
    userEmail = "brian@brianthicks.com";

    aliases = {
      aa = "add --all";
      amend = "commit --amend";
      bclone = ''
        !"bclone() { mkdir -p ~/code/$1; git clone git@git.bytes.zone:$1.git ~/code/$1; }; bclone"'';
      ci = "commit";
      co = "checkout";
      dc = "diff --cached";
      di = "diff";
      hclone = ''
        !"hclone() { mkdir -p ~/code/$1; git clone git@github.com:$1.git ~/code/$1; }; hclone"'';
      publish = "push -u origin HEAD";
      st = "status";
      yoda = "push --force-with-lease";
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
      ".direnv"
      ".python-version"
      ".ruby-version"
      ".vagrant"
      "TAGS"
    ];
  };

  home.packages = with pkgs; [ git-lfs lazygit ];
}
