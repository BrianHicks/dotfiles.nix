{ pkgs, ... }:
{
  programs.mcp = {
    enable = true;
    servers = {
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
}
