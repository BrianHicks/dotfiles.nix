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

  programs.opencode = {
    enable = true;
    enableMcpIntegration = true;

    agents = ./opencode/agents;
    commands = ./opencode/commands;
    tools = ./opencode/tools;
    skills = ./skills;

    context = builtins.readFile ./browser-automation-context.md;

    settings = {
      provider.omlx = {
        npm = "@ai-sdk/openai-compatible";
        name = "oMLX (local)";
        options.baseURL = "http://localhost:10378/v1";

        models."Qwen3.6-35B-a3B-4bit".name = "oMLX: Qwen 3.6 35B a3B";
        models."Qwen3.8-27B-4bit".name = "oMLX: Qwen 3.8 27B";
      };
    };
  };

  home.packages = [
    pkgs.openspec
    pkgs.crit
    pkgs.agent-browser
    pkgs.ncr
  ];
}
