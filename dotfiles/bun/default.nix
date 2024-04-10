{ ... }: {
  programs.bun = {
    enable = true;
    enableGitIntegration = true;

    settings = {
      telemetry = false;
      install.exact = true;
    };
  };
}
