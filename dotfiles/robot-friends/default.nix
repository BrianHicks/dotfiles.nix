{ pkgs, ... }:
{
  # For claude-code
  nixpkgs.config.allowUnfree = true;

  homebrew.formulae = [ "ollama-app" ];

  programs.mcp = {
    enable = true;
    servers = {
      context7.url = "https://mcp.context7.com/mcp";
      gitlab = {
        command = "${pkgs.glab}/bin/glab";
        args = [
          "mcp"
          "serve"
        ];
        headers = { };
      };
      honeycomb.url = "https://mcp.honeycomb.io/mcp";
      sentry.url = "https://mcp.sentry.dev/mcp";
    };
  };

  programs.claude-code = {
    enable = true;
    enableMcpIntegration = true;

    commandsDir = ./commands;
  };

  programs.opencode = {
    enable = true;
    enableMcpIntegration = true;

    agents = ./opencode/agents;
    commands = ./opencode/commands;
    tools = ./opencode/tools;
  };

  home.packages = [ pkgs.openspec ];
}
