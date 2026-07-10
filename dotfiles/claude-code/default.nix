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

    context = ''
      ## Browser Automation

      Use `agent-browser` for web automation. Run `agent-browser --help` for all commands.

      Core workflow:
      1. `agent-browser open <url>` - Navigate to page
      2. `agent-browser snapshot -i` - Get interactive elements with refs (@e1, @e2)
      3. `agent-browser click @e1` / `fill @e2 "text"` - Interact using refs
      4. Re-snapshot after page changes
    '';

    plugins = builtins.attrValues plugins;
  };
}
