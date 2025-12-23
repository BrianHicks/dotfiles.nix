{ pkgs, ... }:
{
  home.packages = [ pkgs.diffoscope ];

  home.shellAliases = {
    "dir-diff" = "diffoscope --exclude-directory-metadata=yes";
  };
}
