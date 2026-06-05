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
    skills = ./skills;

    settings = {
      provider.omlx = {
        npm = "@ai-sdk/openai-compatible";
        name = "oMLX (local)";
        options.baseURL = "http://localhost:10378/v1";

        models.gpt-oss-20b-MXFP4-Q8.name = "GPT OSS";
        models."Qwen3.6-35B-a3B-4bit".name = "Qwen 3.6";
        models."gemma-4-26B-a4B-it-MLX-8bit".name = "Gemma 26B 8-bit";
      };
    };
  };

  # if programs.opencode.plugins ever exists, this should move there.
  home.file.".config/opencode/plugins/peon-ping.ts".source =
    "${pkgs.peon-ping.src}/adapters/opencode/peon-ping.ts";

  home.packages = [
    pkgs.openspec
    pkgs.crit
  ];
}
