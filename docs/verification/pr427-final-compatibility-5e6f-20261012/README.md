# Final pinned modelica_models compatibility qualification

All three original compatibility invocations passed at frozen source `5e6f9cb9afec4caf992a370db78c1a5b342d5336`, using the independently sealed final-source `rumoca-msl-profile`. No Cargo build or Root build-cache access occurred.

| Original CI invocation | Actual outcome | Guardian elapsed |
|---|---|---:|
| `Tests.All --mode compile` | Balanced; 12 states, 275 algebraics, 300 equations; compile 2.90 s | 3.395 s |
| `Tests.LieGroupTests.SO2 --mode simulate --stop-time 0.0` | Initialization succeeded; one sample at time zero; seven original assertions, tolerance 1e-10 | 0.454 s |
| `Tests.LieGroupTests.SE2 --mode simulate --stop-time 0.0` | Initialization succeeded; one sample at time zero; six original assertions, tolerance 1e-9 | 0.408 s |

The corpus remains pinned to `CogniPilot/modelica_models` commit `a41f7c0c00b55c1bf54f03c9b66b901ba8e43c6f`. All 498 tracked input files and all archived source bytes remained unchanged. The archive contains 4,165 regular files plus the separately verified `docs/dev-guide/live` symlink: all 4,166 Git tree entries match the final source. The profile executable hash was checked before and after execution and matches its source-bound build receipt.

Each serial invocation retained its 600-second limit, 4-GiB aggregate resident-plus-swap limit, 16-GiB host reserve and 8-GiB cache/scratch limits. Every guardian exited zero without a signal or resource stop; swap was zero. Models, assertions, tolerances and command arguments were unchanged, with no retries or exclusions.

This qualifies the exact three commands in the mandatory `modelica_models Compatibility Gate` job. SO2 and SE2 prove their original initialization smoke, not full-horizon simulation or simulation of every corpus model. Final ordinary OMC differential semantics, broad quick/full gates and remote CI remain separate evidence. The [ModelicaTest receipt](../pr427-final-modelicatest-5e6f-20261012/README.md) independently qualifies the one-model semantic/parity route.

The compact [receipt](receipt.json) binds the commands, exact source, pinned corpus, sealed executable, scalar resource observations and immutable raw logs by SHA256. Paths are relative to `$HOME`; full guardian process histories remain outside the repository.
