{ pkgs, ... }:
{
  # For claude-code
  nixpkgs.config.allowUnfree = true;

  home.packages = [
    pkgs.gemini-cli
    pkgs.claude-code
  ];
}
