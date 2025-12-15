{ pkgs, ... }:
{
  home.packages = [ pkgs.gh ];

  home.shellAliases = {
    gh = "op plugin run -- gh";
  };
}
