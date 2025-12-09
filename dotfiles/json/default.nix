{ pkgs, ... }: {
  home.packages = [ pkgs.jq pkgs.jless ];
}
