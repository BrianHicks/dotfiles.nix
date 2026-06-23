{ pkgs, ... }: {
  services.llama-cpp = {
    enable = true;
    package = pkgs.ik-llama-cpp;

    settings = {
      port = 8081;

      model = "Qwen3.6-35B-A3B-IQ4_KS_mixed.gguf";

      # reduce repetition etc
      temp = 0.7;
      top-k = 20;
      top-p = 0.95;
      min-p = 0;
      presence-penalty = 1.5;
      frequency-penalty = 0.1;

      special = true;
      jinja = true;
      ctx-size = 23768;
      flash-attn = "on";
      merge-up-gate-experts = true;
      mlock = true;
      run-time-repack = true;
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
    host = "0.0.0.0";
    openFirewall = true;
    environment = {
      ENABLE_AUTOCOMPLETE_GENERATION = "False";
      ENABLE_FOLLOW_UP_GENERATION = "False";
      ENABLE_TITLE_GENERATION = "False";
      ENABLE_TAGS_GENERATION = "False";
    };
  };
}
