{ pkgs, ... }: {
  services.llama-cpp = {
    # unsloth/Qwen3.6-35B-A3B-MTP-GGUF:UD-Q4_K_M
    # with no args: 15.9 t/s encode, 8.8 t/s decode
    #
    # --temp 1.0 --top-p 0.95 --top-k 20 --min-p 0.00 --spec-type draft-mtp --spec-draft-n-max 2
    # 21.7 t/s encode, 10.6 t/s decode
    #
    # unsloth/Qwen3.6-27B-GGUF:UD-Q4_K_XL with --temp 1.0 --top-p 0.95 --top-k 20 --min-p 0.00
    # 4.0 t/s encode, 1.7 t/s decode
    #
    # unsloth/Qwen3.6-35B-A3B-GGUF:UD-Q4_K_XL --temp 1.0 --top-p 0.95 --top-k 20 --min-p 0.00
    # 21.8 t/s encode, 9.4 t/s decode
    #
    # unsloth/Qwen3.6-27B-MTP-GGUF:UD-Q4_K_XL --temp 1.0 --top-p 0.95 --top-k 20 --min-p 0.00 --spec-type draft-mtp --spec-draft-n-max 2
    # 3.5 t/s encoe, 1.9 t/s decode
    enable = true;
    settings.port = 8081;
    package = pkgs.ik-llama-cpp;
  };

  services.ollama = {
    enable = false;
    # looks like the default uses 15 CPUs by default (of 16 on Avior)
    #
    # with qwen3.6:35b MOE Q4_K_M:
    # ollama-cpu: 10.43 t/s after 514 decoded
    # ollama-vulkan: 10.43 t/s after 511 decoded
    # with flash attention: 10.39 t/s after 509 decoded
    # with OLLAMA_KV_CACHE_TYPE=q8_0: 10.34 t/s after 505 decoded
    # with OLLAMA_IGPU_ENABLE=1: 10.12 t/s after 529 decoded
    #
    # with qwen3.6:27b Q4_K_M:
    # q8_0 and no igpu enable: 1.68 t/s after 513 decoded
    # ah, it's a dense model. More to load. OK, not a great fit for this arch
    # maybe. Need the whole model to be loaded in context and perhaps swapping.
    # Would potentially be possible to use llama.cpp to get a good config but
    # IDK if that's worth it.
    #
    # with hf.co/unsloth/Qwen3.6-35B-A3B-MTP-GGUF:UD-Q4_K_M:
    # misconfigured somehow. Seems to just see `{{ .Prompt }}` as the prompt, so
    # maybe some errors with ollama. That said... q8_0 and no igpu: 8.93 t/s
    # after 535 decoded. I think it would be faster if there were the correct
    # args, though.
    environmentVariables = {
      OLLAMA_CONTEXT_LENGTH = "32768";
      OLLAMA_FLASH_ATTENTION = "1";
      OLLAMA_IGPU_ENABLE = "1";
    };
  };

  services.open-webui = {
    enable = true;
  };
}
