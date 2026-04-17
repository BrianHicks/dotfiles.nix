{ pkgs, ... }:
{
  home.packages = [ pkgs.glab ];

  home.shellAliases = {
    "glab.mr" = "glab mr create --push";
  };
}
