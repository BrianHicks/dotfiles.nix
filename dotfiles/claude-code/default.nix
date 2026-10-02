{ pkgs, lib, ... }:
let
  plugins = {
    crit-integration = "${pkgs.crit.src}/integrations/claude-code";
    learning-opportunities = "${pkgs.learning-opportunities}/learning-opportunities";
    learning-opportunities-auto = "${pkgs.learning-opportunities}/learning-opportunities-auto";
  };
in
{
  # For claude-code
  nixpkgs.config.allowUnfree = true;

  assertions = map (attr: {
    message = "claude-code: plugin \`${attr.name}\` path must exist";
    assertion = builtins.pathExists attr.value;
  }) (lib.attrsToList plugins);

  programs.claude-code = {
    enable = true;
    enableMcpIntegration = true;

    commandsDir = ./commands;
    skills = ../robot-friends/skills;

    context = builtins.readFile ../robot-friends/browser-automation-context.md;

    plugins = plugins;
  };
}
