{ pkgs, config, ... }: {
  services.llama-cpp = {
    enable = true;
    package = pkgs.ik-llama-cpp;

    settings = {
      port = 8081;

      model = "Qwen3.6-35B-A3B-IQ4_KS_mixed.gguf";

      special = true;
      jinja = true;
      ctx-size = 23768;
      flash-attn = "on";
      merge-up-gate-experts = true;
      mlock = true;
      run-time-repack = true;
      temp = 0.7;
      cache-type-k = "q8_0";
      cache-type-v = "q8_0";
      scheduler_async = true;
      split-mode-graph-scheduling = true;
      threads = 6;
      threads-batch = 14;
    };
  };

  # Bump RLIMIT to the amount we need. Qwen3.6 needs 19.3gb. Llama-cpp will
  # print out the value it wants to mlock if it can't. Giving it a little extra
  # (20gb) just to be safe.
  systemd.services.llama-cpp.serviceConfig.LimitMEMLOCK = "20000000000";

  services.open-webui = {
    enable = true;
    host = "192.168.0.240";
    openFirewall = true;
    environment = {
      ENABLE_AUTOCOMPLETE_GENERATION = "False";
      ENABLE_FOLLOW_UP_GENERATION = "False";
      ENABLE_TITLE_GENERATION = "False";
      ENABLE_TAGS_GENERATION = "False";
    };
  };

  services.librechat = {
    enable = false; # enabling makes GHA build Mongo; heads up!
    enableLocalDB = true;
    credentialsFile = "/run/secrets/librechat"; # TODO: back up
    env = {
      HOST = "192.168.0.240";
      PORT = 3080;
      ALLOW_EMAIL_LOGIN = "true";
      ALLOW_REGISTRATION = "true";
    };
    settings = {
      version = "1.3.13";
      cache = true;

      endpoints.custom = [
        {
          name = "llama.cpp";
          apiKey = "";
          baseURL = "http://127.0.0.1:${toString config.services.llama-cpp.settings.port}/v1";
          models = {
            default = [ config.services.llama-cpp.settings.model ];
            fetch = false;
          };

          titleConvo = true;
          titleModel = "current_model";

          modelDisplayLabel = "Qwen";
        }
      ];

      registration.allowedDomains = [ "brianthicks.com" ];
    };
  };

  networking.firewall.allowedTCPPorts = [ 3080 ];
}
