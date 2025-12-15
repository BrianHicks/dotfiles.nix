{ pkgs, ... }:
{
  home.packages = [ pkgs.glab ];

  home.shellAliases = {
    # 1Password plugins
    glab = "op plugin run -- glab";
    "glab.mr" = "glab mr create --push";
  };
}
