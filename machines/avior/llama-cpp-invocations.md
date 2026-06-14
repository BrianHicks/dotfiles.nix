# Llama-CPP invocations

Test prompt: `how many rs in strrawberry`. Qwen3.6 knows this trick, but misspelling it makes it second-guess just enough to get 512-1024 tokens of output.

## Plain Invocation

Without any ik-llama-cpp flags...

```console
$ llama-cli --model ~/.cache/huggingface/hub/models--unsloth--Qwen3.6-35B-A3B-GGUF/snapshots/a483e9e6cbd595906af30beda3187c2663a1118c/Qwen3.6-35B-A3B-UD-Q4_K_XL.gguf -p 'how many rs in strrawberry'
llama_print_timings:        load time =   15878.69 ms
llama_print_timings:      sample time =      83.76 ms /   694 runs   (    0.12 ms per token,  8285.58 tokens per second)
llama_print_timings: prompt eval time =     206.49 ms /     7 tokens (   29.50 ms per token,    33.90 tokens per second)
llama_print_timings:        eval time =   61821.15 ms /   693 runs   (   89.21 ms per token,    11.21 tokens per second)
llama_print_timings:       total time =   62342.26 ms /   700 tokens
```

## MTP

Shouldn't really help with MOE but let's benchmark it anyway.

```console
$ llama-cli --model ~/.cache/huggingface/hub/models--unsloth--Qwen3.6-35B-A3B-GGUF/snapshots/a483e9e6cbd595906af30beda3187c2663a1118c/Qwen3.6-35B-A3B-UD-Q4_K_XL.gguf -p 'how many rs in strrawberry' --spec-type mtp:n_max=3,p_min=0.0 --spec-autotune
llama_print_timings:        load time =    4821.38 ms
llama_print_timings:      sample time =      55.57 ms /   454 runs   (    0.12 ms per token,  8169.58 tokens per second)
llama_print_timings: prompt eval time =     280.75 ms /     7 tokens (   40.11 ms per token,    24.93 tokens per second)
llama_print_timings:        eval time =   39549.38 ms /   453 runs   (   87.31 ms per token,    11.45 tokens per second)
llama_print_timings:       total time =   40042.01 ms /   460 tokens
```

May be unhelpful that I don't have an MTP specific model? But I downloaded that one, so let's try it too.

```console
$ llama-cli --model ~/.cache/huggingface/hub/models--unsloth--Qwen3.6-35B-A3B-MTP-GGUF/snapshots/5bc3e238d916f48a861bac2f8a1990a0e9b7e98d/Qwen3.6-35B-A3B-UD-Q4_K_M.gguf -p 'how many rs in strrawberry' --spec-type mtp:n_max=3,p_min=0.0 --spec-autotune
llama_print_timings:        load time =   16563.52 ms
llama_print_timings:      sample time =      64.29 ms /   583 runs   (    0.11 ms per token,  9067.86 tokens per second)
llama_print_timings: prompt eval time =     247.26 ms /     7 tokens (   35.32 ms per token,    28.31 tokens per second)
llama_print_timings:        eval time =   49485.42 ms /   582 runs   (   85.03 ms per token,    11.76 tokens per second)
llama_print_timings:       total time =   49982.07 ms /   589 tokens
```

So maybe *slightly* faster, but I'm also not sure what the variance is here and it might be within the error bars. Would need to repeat a bunch to be sure but I don't think it's worth if if the improvement is really 0.1-0.3 TPS.

## Flags from [Gemma 4 on a 2016 Xeon](https://point.free/blog/gemma-4-on-a-2016-xeon/)

Except I'm using 6 CPUs to match the asymmetrical cores I have on this system...

```console
$ llama-cli --model ~/.cache/huggingface/hub/models--unsloth--Qwen3.6-35B-A3B-GGUF/snapshots/a483e9e6cbd595906af30beda3187c2663a1118c/Qwen3.6-35B-A3B-UD-Q4_K_XL.gguf -p 'how many rs in strrawberry' --color --special --jinja -smgs -sas -mea 256 --split-mode-f32 --temp 0.7 -t 6 --parallel 6 --cpu-moe --merge-up-gate-experts --flash-attn on --mla-use 3 --mlock --run-time-repack --no-kv-offload
llama_print_timings:        load time =   33919.87 ms
llama_print_timings:      sample time =     127.42 ms /   659 runs   (    0.19 ms per token,  5171.95 tokens per second)
llama_print_timings: prompt eval time =     195.03 ms /     7 tokens (   27.86 ms per token,    35.89 tokens per second)
llama_print_timings:        eval time =   48882.42 ms /   658 runs   (   74.29 ms per token,    13.46 tokens per second)
llama_print_timings:       total time =   49616.34 ms /   665 tokens
```

With 13 of the 14 cores, I get these stats. Yeah, slower.

```
llama_print_timings:        load time =   25239.82 ms
llama_print_timings:      sample time =     195.83 ms /   699 runs   (    0.28 ms per token,  3569.48 tokens per second)
llama_print_timings: prompt eval time =     536.84 ms /     7 tokens (   76.69 ms per token,    13.04 tokens per second)
llama_print_timings:        eval time =  107318.88 ms /   698 runs   (  153.75 ms per token,     6.50 tokens per second)
llama_print_timings:       total time =  108389.19 ms /   705 tokens
```

This also fails to lock memory. Let's take care of that real quick and run again with 6 cores...

```console
llama_print_timings:        load time =   23405.10 ms
llama_print_timings:      sample time =     117.49 ms /   617 runs   (    0.19 ms per token,  5251.56 tokens per second)
llama_print_timings: prompt eval time =     123.63 ms /     7 tokens (   17.66 ms per token,    56.62 tokens per second)
llama_print_timings:        eval time =   46794.91 ms /   616 runs   (   75.97 ms per token,    13.16 tokens per second)
llama_print_timings:       total time =   47294.44 ms /   623 tokens
```

Not significantly fast, but good to know.

Note: post has `-sm graph` but it fails with this message:

```
lama_model_load: error loading model: It is not possible to use split mode 'graph' with less than 2 devices
llama_model_load_from_file: failed to load model
```

So I'm not sure what they were seeing but that flag seems to be exclusively for splitting work across multiple GPUs. Not relevant here, or at least not relevant in the way it was for the post.

Edit later: ah, the server they're working with has 8 CPUs socketed. I have one CPU socketed. One device after all.

## Command I ran late 2026-06-14

Based on some advice from LLMs...

```console
$ llama-cli --model ~/.cache/huggingface/hub/models--unsloth--Qwen3.6-35B-A3B-GGUF/snapshots/a483e9e6cbd595906af30beda3187c2663a1118c/Qwen3.6-35B-A3B-UD-Q4_K_XL.gguf -t 6 -tb 14 --ctx-size 32768 -b 2048 -ub 2048 --flash-attn on --no-mmap --mlock --run-time-repack --cpu-moe --merge-up-gate-experts -ctk q8_0 -ctv q8_0 --jinja -p 'how many rs in strrawberry'
llama_print_timings:        load time =   12341.45 ms
llama_print_timings:      sample time =     128.87 ms /   668 runs   (    0.19 ms per token,  5183.36 tokens per second)
llama_print_timings: prompt eval time =     582.34 ms /     7 tokens (   83.19 ms per token,    12.02 tokens per second)
llama_print_timings:        eval time =   51431.57 ms /   667 runs   (   77.11 ms per token,    12.97 tokens per second)
llama_print_timings:       total time =   52431.93 ms /   674 tokens
```

hmm... I remember getting around 15 tps from that. However, I was doing it without gnome logged in. Could be it?

So what's the difference between that and the one above? Can I combine them?

| Flag                    | Post    | Above   |
|-------------------------|---------|---------|
| --cpu-moe               | present | present |
| --ctx-size              | absent  | 32768   |
| --flash-attn            | on      | on      |
| --jinja                 | present | present |
| --merge-up-gate-experts | present | present |
| --mla-use               | 3       | absent  |
| --mlock                 | present | present |
| --no-kv-offload         | present | absent  |
| --parallel              | 6       | absent  |
| --run-time-repack       | present | present |
| --split-mode-f32        | present | absent  |
| --temp                  | 0.7     | absent  |
| -b                      | absent  | 2048    |
| -ctk                    | absent  | q8_0    |
| -ctv                    | absent  | q8_0    |
| -mea                    | 256     | absent  |
| -sas                    | present | absent  |
| -smgs                   | present | absent  |
| -t                      | 6       | 6       |
| -tb                     | absent  | 14      |
| -ub                     | absent  | 2048    |

So a combined command might look like:

```console
$ llama-cli --model ~/.cache/huggingface/hub/models--unsloth--Qwen3.6-35B-A3B-GGUF/snapshots/a483e9e6cbd595906af30beda3187c2663a1118c/Qwen3.6-35B-A3B-UD-Q4_K_XL.gguf -p 'how many rs in strrawberry' --color --special --jinja --cpu-moe --ctx-size 23768 --flash-attn on --jinja --merge-up-gate-experts --mla-use 3 --mlock --parallel 6 --run-time-repack --split-mode-f32 --temp 0.7 -b 2048 -ctk q8_0 -ctv q8_0 -mea 256 -sas -smgs -t 6 -tb 14 -ub 2048
llama_print_timings:        load time =   13076.48 ms
llama_print_timings:      sample time =     202.18 ms /  1023 runs   (    0.20 ms per token,  5059.95 tokens per second)
llama_print_timings: prompt eval time =     567.22 ms /     7 tokens (   81.03 ms per token,    12.34 tokens per second)
llama_print_timings:        eval time =   76535.47 ms /  1022 runs   (   74.89 ms per token,    13.35 tokens per second)
llama_print_timings:       total time =   77750.87 ms /  1029 tokens
```

So not like... a lot faster. I think I'm approaching the limit of what's possible.

## Pinning work to CPUs

One last thing, though: I'm going to pin the work to the strong CPUs.

Here's what `lscpu` has to say:

```
CPU NODE SOCKET CORE L1d:L1i:L2:L3 ONLINE    MAXMHZ   MINMHZ       MHZ
  0    0      0    0 0:0:0:0          yes 5100.0000 400.0000 4199.8408
  1    0      0    1 4:4:1:0          yes 5100.0000 400.0000 1052.0560
  2    0      0    2 16:16:4:0        yes 5100.0000 400.0000 3894.9751
  3    0      0    3 20:20:5:0        yes 5100.0000 400.0000 3400.0000
  4    0      0    4 24:24:6:0        yes 5100.0000 400.0000 3599.6321
  5    0      0    5 28:28:7:0        yes 5100.0000 400.0000 3600.0000
  6    0      0    6 2:0              yes 4400.0000 400.0000  400.0000
  7    0      0    7 18:18:2:0        yes 4400.0000 400.0000 3848.7771
  8    0      0    8 2:0              yes 4400.0000 400.0000 4126.5620
  9    0      0    9 22:22:2:0        yes 4400.0000 400.0000 3822.7639
 10    0      0   10 3:0              yes 4400.0000 400.0000 3647.3220
 11    0      0   11 26:26:3:0        yes 4400.0000 400.0000 3957.1270
 12    0      0   12 3:0              yes 4400.0000 400.0000 3708.8589
 13    0      0   13 30:30:3:0        yes 4400.0000 400.0000 3774.4419
 14    0      0   14 64:64:8          yes 2500.0000 400.0000  745.6250
 15    0      0   15 66:66:8          yes 2500.0000 400.0000 1010.7890
 ```

 So maybe CPUs 0-5?

```console
$ taskset --cpu-list 0-5 llama-cli --model ~/.cache/huggingface/hub/models--unsloth--Qwen3.6-35B-A3B-GGUF/snapshots/a483e9e6cbd595906af30beda3187c2663a1118c/Qwen3.6-35B-A3B-UD-Q4_K_XL.gguf -p 'how many rs in strrawberry' --color --special --jinja --cpu-moe --ctx-size 23768 --flash-attn on --jinja --merge-up-gate-experts --mla-use 3 --mlock --parallel 6 --run-time-repack --split-mode-f32 --temp 0.7 -b 2048 -ctk q8_0 -ctv q8_0 -mea 256 -sas -smgs -t 6 -ub 2048
llama_print_timings:        load time =   12669.92 ms
llama_print_timings:      sample time =      99.59 ms /   919 runs   (    0.11 ms per token,  9228.02 tokens per second)
llama_print_timings: prompt eval time =     106.61 ms /     7 tokens (   15.23 ms per token,    65.66 tokens per second)
llama_print_timings:        eval time =   58937.67 ms /   918 runs   (   64.20 ms per token,    15.58 tokens per second)
llama_print_timings:       total time =   59435.82 ms /   925 tokens
```

Note that I dropped `-tb 16`.

Let's also try it with the first 14 CPUs:

```console
$ taskset --cpu-list 0-13 llama-cli --model ~/.cache/huggingface/hub/models--unsloth--Qwen3.6-35B-A3B-GGUF/snapshots/a483e9e6cbd595906af30beda3187c2663a1118c/Qwen3.6-35B-A3B-UD-Q4_K_XL.gguf -p 'how many rs in strrawberry' --color --special --jinja --cpu-moe --ctx-size 23768 --flash-attn on --jinja --merge-up-gate-experts --mla-use 3 --mlock --parallel 14 --run-time-repack --split-mode-f32 --temp 0.7 -b 2048 -ctk q8_0 -ctv q8_0 -mea 256 -sas -smgs -t 14 -ub 2048
llama_print_timings:        load time =   12775.95 ms
llama_print_timings:      sample time =     155.59 ms /   646 runs   (    0.24 ms per token,  4151.94 tokens per second)
llama_print_timings: prompt eval time =     241.98 ms /     7 tokens (   34.57 ms per token,    28.93 tokens per second)
llama_print_timings:        eval time =   73110.46 ms /   645 runs   (  113.35 ms per token,     8.82 tokens per second)
llama_print_timings:       total time =   73840.38 ms /   652 tokens
```

Not nearly as good. So fully saturating the power cores is much faster than 50% CPU load across the board (observed in last command.)

Let's fan it out to 1 through 6 cores, just to see if the scaling is linear or what. Just listing generated TPS here. I'll also do the 7th and 8th cores just to see if it drops off there as I expect.

| Cores |   TPS | CPU Utilization (eyballed) |
|------:|------:|---------------------------:|
| 1     |  5.64 |                       100% |
| 2     | 11.98 |                       100% |
| 3     | 14.24 |                        92% |
| 4     | 15.49 |                        90% |
| 5     | 15.31 |                        81% |
| 6     | 15.01 |                        75% |
| 7     | 13.95 |    70% on 1-6, 75-80% on 7 |
| 8     | 13.25 |  65% on 1-6, 70-75% on 7-8 |

So yeah, pretty sharp falloff when you get to the smaller cores.
