{ pkgs, ... }:

{
  programs.git = {
    enable = true;

    userName = "Brian Hicks";
    userEmail = "brian@brianthicks.com";

    aliases = {
      aa = "add --all";
      ci = "commit";
      dc = "diff --cached";
      di = "diff";
      st = "status";
    };

    # todo: extraConfig with lfs

    # todo: global ignores
  };

  home.packages = with pkgs; [ git-lfs ];
}
