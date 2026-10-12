# FMI C pulse lifecycle qualification

The original workspace warning test failed at source `6d62feb2a` because the FMI C profile refused the checked `PulseConditionMemory` row of `StateAndTimeEvents`. The model and original six model/target compilation cells remain unchanged.

Qualified source: `95867919f749202f36563367f72074a06717f1f3` (base `1a197650204715a97f23a8284a6d6f8ea077a1eb`). The private FMI projection derives pulse rows and their exact checked producers once. The shared FMI2/FMI3 lifecycle evaluates those producers with clocks off after event actions and before history commit, restores clock lanes on both success and error, and publishes only pulse targets. Ordinary relation and condition memories retain their settled values. This implements the existing [ME-EVENT-002 contract](../../../spec/SPEC_0044_FMI_EXECUTION_CATALOG.md).

| Actual gate | Result |
|---|---|
| Original optimized generated-C warning test | PASS: three unchanged models × FMI2/FMI3 × O2/O3; all translation units with warnings denied |
| Public emitted-C pulse lifecycle | PASS: FMI2 and FMI3 Co-Simulation, one independent six-tick oracle with state/time-event coexistence |
| Full Solve IR library | 609 PASS, zero failed/ignored |
| Scoped Clippy | PASS: IR-Solve library and Rumoca `suite_core`, all features, warnings denied |

Both public ABI traces have `ticks=1..6`; state/time counters remain zero through 0.4, become one at 0.5, and remain one at 0.6. Initialization checks all three counters are zero. The FMI3 step status, completion flags and returned time are checked too. Identifiers and value references come from the actual rendered XML.

All 4,063 tracked source hashes matched before and after execution. Each gate retained the unchanged 600-second, 4-GiB aggregate resident-plus-swap, 8-GiB cache, 16-GiB host reserve and 8-GiB scratch reserve limits, with four jobs/threads and no incremental/debug output. No resource stop occurred. The runtime gate took 251.85 seconds including compilation (12.29 seconds in tests), at a peak 3,024,296 KiB resident-plus-swap.

The [adjacent receipt](fmi-pulse-lifecycle-qualification.json) binds commands, raw logs, guardian results, source seals and issued Rust test executables. Intermediate Clippy line-budget refusals and the first FMI2-only execution are retained separately. This proves emitted-C execution and compilation; packaged FMUs, Model Exchange runtime, the complete workspace and current remote CI still require their own integrated-source gates.
