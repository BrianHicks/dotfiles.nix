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

    context = ''
      ## Browser Automation

      Use `agent-browser` for web automation. Run `agent-browser --help` for all commands.

      Core workflow:
      1. `agent-browser open <url>` - Navigate to page
      2. `agent-browser snapshot -i` - Get interactive elements with refs (@e1, @e2)
      3. `agent-browser click @e1` / `fill @e2 "text"` - Interact using refs
      4. Re-snapshot after page changes
    '';

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
    pkgs.rtk # needs manual setup: rtk init --opencode -g
  ];
}
