#!/usr/bin/env bash
set -euo pipefail

DEST="q-src"

# Download all the files (huge)
hf download unsloth/Qwen3.6-35B-A3B-GGUF --include 'BF16*' --include 'imatrix_unsloth.gguf_file' --local-dir "$DEST"

# Convert the imatrix to the right format for quantization
llama-imatrix --in-file "$DEST/imatrix_unsloth.gguf_file" --output-format dat -o "$DEST/imatrix_unsloth.dat" -m "$DEST/BF16/Qwen3.6-35B-A3B-BF16-00001-of-00002.gguf"
