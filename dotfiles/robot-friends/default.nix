{ pkgs, ... }:
{
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

  programs.peon-ping = {
    enable = true;
    enableZshIntegration = true;

    settings = {
      default_pack = "peon";
      volume = 1.0;
    };

    installPacks = [ "peon" ];
  };

  programs.opencode = {
    enable = true;
    enableMcpIntegration = true;

    agents = ./opencode/agents;
    commands = ./opencode/commands;
    tools = ./opencode/tools;
  };

  # if programs.opencode.plugins ever exists, this should move there.
  home.file.".config/opencode/plugins/peon-ping.ts".source =
    "${pkgs.peon-ping.src}/adapters/opencode/peon-ping.ts";

  home.packages = [
    pkgs.openspec
    pkgs.crit
  ];
}
