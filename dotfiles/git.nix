{ pkgs, ... }:
let
  deltaTheme = ''--theme=1337 --plus-color="#32473d" --minus-color="#643632"'';
in {
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

      # delta
      core.pager = "${pkgs.gitAndTools.delta}/bin/delta ${deltaTheme}";
      interactive.diffFilter =
        "${pkgs.gitAndTools.delta}/bin/delta --color-only ${deltaTheme}";
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
    };

  home.packages = with pkgs; [ git-lfs lazygit pkgs.gitAndTools.delta ];
}
