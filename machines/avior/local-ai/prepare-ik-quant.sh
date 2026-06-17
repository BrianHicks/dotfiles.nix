#!/usr/bin/env bash
set -euo pipefail

# llama-quantize \
#   --imatrix /home/brian/q-src/imatrix_unsloth.dat \
#   --custom-q "ffn_(gate|up|down)_exps=iq4_ks" \
#   /home/brian/q-src/BF16/Qwen3.6-35B-A3B-BF16-00001-of-00002.gguf \
#   ./Qwen3.6-35B-A3B-IQ4_KS.gguf \
#   Q6_0

llama-quantize \
  --imatrix /home/brian/q-src/imatrix_unsloth.dat \
  /home/brian/q-src/BF16/Qwen3.6-35B-A3B-BF16-00001-of-00002.gguf \
  ./Qwen3.6-35B-A3B-IQ4_KS_all.gguf \
  IQ4_KS
