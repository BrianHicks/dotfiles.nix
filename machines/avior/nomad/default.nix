{ pkgs, config, ... }: {
  age.secrets."global-server-nomad-key.pem".file = ../../../secrets/global-server-nomad-key.pem.age;

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

      acl.enabled = true;

      tls = [
        {
          http = true;
          rpc = true;

          ca_file = ./nomad-agent-ca.pem;
          cert_file = ./global-server-nomad.pem;
          key_file = config.age.secrets."global-server-nomad-key.pem".path;

          verify_server_hostname = true;
          verify_https_client = false;
        }
      ];

      plugin = [
        {
          nomad-driver-podman = {
            config = [
              {
                # https://github.com/hashicorp/nomad-driver-podman
              }
            ];
          };
        }
      ];

      telemetry = [
        {
          publish_allocation_metrics = true;
          publish_node_metrics = true;
        }
      ];
    };
  };
}
