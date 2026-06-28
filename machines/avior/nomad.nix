{ pkgs, config, ... }: {
  services.nomad = {
    enable = true;

    # Would rather not have the Docker daemon running if I can avoid it!
    enableDocker = false;
    extraSettingsPlugins = [ pkgs.nomad-driver-podman ];

    # need to access podman socket
    dropPrivileges = false;

    settings = {
      bind_addr = "{{ GetInterfaceIP \"tailscale0\" }}";

      server = {
        enabled = true;
        bootstrap_expect = 1;
      };

      client = {
        enabled = true;
        node_class = "internal";
        alloc_mounts_dir = "${config.services.nomad.settings.data_dir}/alloc_mounts";

        # this is apparently an outdated way to stop a driver from loading,
        # but... it still works!
        options."driver.blacklist" = "docker";
      };

      plugin = [
        {
          nomad-driver-podman = {
            config = [{
              # https://github.com/hashicorp/nomad-driver-podman
            }];
          };
        }
      ];
    };
  };
}
