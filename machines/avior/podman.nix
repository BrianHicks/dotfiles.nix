{ ... }: {
  virtualisation = {
    containers.enable = true;
    podman = {
      enable = true;
      defaultNetwork.settings.dns_enabled = true;

      # use shims but don't need the socket
      dockerCompat = true;
      dockerSocket.enable = false;
    };
  };
}
