{ pkgs, ... }:
{
  home.packages = [ (pkgs.diffoscope.overrideAttrs (old: { doCheck = false; })) ];

  home.shellAliases = {
    "dir-diff" = "diffoscope --exclude-directory-metadata=yes";
  };
}
