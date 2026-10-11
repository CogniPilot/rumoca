# Strict C and Modelica exports at 86c

Exact source `86c3bc1a6128e3d3174fefa169ae7bd5e03243e9` passed both canonical Linux commands:

```sh
cargo xtask verify template-runtimes --backend c --require-external-tools
cargo xtask verify template-runtimes --backend modelica --require-external-tools
```

C selected **11 + 1 tests**, all passed with zero failed/ignored. This includes the five unchanged workshop models’ initialized/RHS numeric controls, selected-region faults, exact Integer transport, bounded loops, tensor calls and the compacted algebraic-family call. Modelica selected **four tests**, all passed with zero failed/ignored. The two workshop tests completed **28 OMC cells**: six unchanged models through original/base/flat/DAE targets (24), plus the derivative-coordinate original and three exports (4). The other two tests check compiler roundtrip and required OMC admission. These counts are distinct.

The first C attempt stopped before tests because the original local guardian refused any nonzero `VmSwap`; its PID/swap amount was not recorded. The first Modelica attempt timed out at 600 seconds after 586 seconds of compilation, three named tests and 16 workshop OMC cells, with no final Rust summary. Both remain **unqualified**. They were not semantic failures. The original source, models, tolerances and commands were retained.

ROOT authorized distinctly named warm export invocations under a corrected resource policy: aggregate resident RSS **plus swap** must remain below the same 4 GiB budget, with per-process PID/birth/time observations retained. The 600-second limit, 16-GiB host reserve, 8-GiB target cap and 8-GiB scratch floor stayed unchanged. The original zero-swap guardian and failed receipts are immutable. Both qualified attempts observed zero swap and no resource stop.

| Qualified command | Guarded elapsed time | Peak aggregate RSS + swap |
|---|---:|---:|
| C | 71.258 s | 2,260,484 KiB |
| Modelica | 72.517 s | 1,642,708 KiB |

These are warm command timings including Cargo, not cold-build or performance claims. Environment jobs/test threads/Rayon were 4; the canonical xtask retains its explicit one-job inner Cargo command. GCC 15.2.0, Rust 1.95 nightly and OMC `a96aa1a-cmake` came from the source-pinned Nix full shell. No workspace-wide external-tool override was set.

All 4,037 tracked source entries were checked before and after execution. Actual producer/test ELFs were sealed before canonical artifact trimming; both qualified commands used suite ELF SHA256 `45dca4169d34655a5ebe5033346ab7ee6a7a948b0a439b68bb3d7452fa81e77e`. Fresh terminal audit found no process references to the owned source/target and no remaining strict marker. The initial C count annotation omitted seven tests selected through a module prefix; the qualified receipt explicitly corrects it from the actual complete summaries.

[Portable receipt](strict-c-modelica-86c-qualification.json) binds exact commands, tests, source, tools, recipes, guard, logs, executable identities and each retained OMC source/script/CSV cell. This proves the selected exports at **86c**, not later revisions, full GitHub CI, full authored simulation horizons, or cohort parity.
