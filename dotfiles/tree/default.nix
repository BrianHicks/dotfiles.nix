{ pkgs, ... }:
{
  home.packages = [ pkgs.tree ];
}
