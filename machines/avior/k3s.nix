{ pkgs, config, ... }: {
  age.secrets.k8s-agent-token.file = ../../secrets/tailscale-key.age;

  services.k3s = {
    enable = true;
    role = "server";
    # nodeIP = "...";
    nodeExternalIP = "100.64.0.5";
    extraFlags = [
      "--snapshotter=stargz"
      "--vpn-auth-file=${config.age.secrets.k8s-agent-token.path}"
    ];
    nodeLabel = [ "bytes.zone/visibility=private" ];
  };

  # Not in there by default since VPN auth is experimental.
  systemd.services.k3s.path = [ pkgs.tailscale ];
}
