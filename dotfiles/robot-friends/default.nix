{ pkgs, ... }:
{
  homebrew.formulae = [ "ollama-app" ];

  programs.mcp = {
    enable = true;
    servers = {
      context7.url = "https://mcp.context7.com/mcp";
      mise = {
        command = "${pkgs.mise}/bin/mise";
        args = [ "mcp" ];
      };
    };
  };

  programs.opencode = {
    enable = true;
    enableMcpIntegration = true;

    agents = ./opencode/agents;
    commands = ./opencode/commands;
    tools = ./opencode/tools;
  };

  home.packages = [
    pkgs.openspec
    pkgs.crit
  ];
}
