{ pkgs, ... }:

{
  programs.git = {
    enable = true;

    userName = "Brian Hicks";
    userEmail = "brian@brianthicks.com";

    aliases = {
      st = "status";
      ci = "commit";
      di = "diff";
      dc = "diff --cached";
    };

    # todo: extraConfig with lfs

    # todo: global ignores
  };

  home.packages = with pkgs; [ git-lfs ];
}
