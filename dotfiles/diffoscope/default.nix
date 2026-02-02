{ pkgs, ... }:
{
  home.packages = [
    (pkgs.diffoscope.overridePythonAttrs (old: {
      doCheck = false;
    }))
  ];

  home.shellAliases = {
    "dir-diff" = "diffoscope --exclude-directory-metadata=yes";
  };
}
