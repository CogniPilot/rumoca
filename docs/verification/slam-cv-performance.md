# Native image-family template preparation

Branch: `slam-cv-performance`, based on
`f8efdf225048de9fa3a2992ca7b4f57dda18dafd`.

The native WGSL renderer previously scalarized every compact Map/AffineStencil
family while constructing both the legacy Solve view and the target-specific
block view. Node-list construction also scalarized each family before any
consumer requested that list. Thus a compact RGB averaging family incurred
image-sized fallback construction even though its WGSL kernel stayed small.

`rumoca-phase-codegen` now materializes node views on demand and shares one
cached, checked scalar projection between the legacy programs and scalar plan.
Native family rendering reads the original compact inventory. Errors raised by
an explicitly requested scalar fallback remain rendering errors with their
source. This changes template preparation, not Modelica semantics, equations,
iteration counts, target admission, or GPU execution.

## Reproduction and measurements

Run from this worktree in the pinned default Nix shell, with
`CARGO_BUILD_JOBS=2 RUST_TEST_THREADS=2 RAYON_NUM_THREADS=2` and `nice -n 10`:

```
cargo test -p rumoca-phase-codegen image_sized_native_wgsl_is_extent_independent -- --nocapture
```

A single native AffineStencil family reads the three RGB channels of each pixel,
adds them, and divides by three. The fixture isolates backend preparation; it is
not a source-level claim that stateless RGB/Harris is admitted by `wgsl-ode`.
The table records renderer-context construction in the optimized test profile,
with the original production files restored for the first measurement and the
revised files restored for the second. The same fixture and toolchain were used.

| Pixels | Original context | Revised context | WGSL bytes, both |
| --- | ---: | ---: | ---: |
| 1,024 | 6.351 ms | 0.217 ms | 1,599 |
| 160 × 90 | 85.019 ms | 0.057 ms | 1,607 |
| 320 × 180 | 317.040 ms | 0.059 ms | 1,607 |

First-case cache startup and concurrent host workloads affect these timings.
The deterministic regression is absence of scalar initialization while rendering
native WGSL, bounded shader size, and successful Naga parsing/validation. A
separate test requests fallback programs and verifies their complete canonical
serialization against the shared checked scalarizer, plus reuse of the same
cached program allocation. An invalid stride fixture verifies error propagation.
These are not end-to-end Harris compile timings or GPU throughput measurements.

## WASM compile response interoperability

The compile binding injected `__rumoca_build` into both canonical DAE payloads.
Their own strict decoder rejected that extension when passed to `render_target`.
The producer now attaches the same metadata to the compile response envelope;
`dae` and `dae_native` remain identical canonical DAE payloads. The new CvMap
regression checks both payloads with the canonical decoder and exact serialized
roundtrip, preserving integer source IDs.

Clients reading `response.dae.__rumoca_build` must read
`response.__rumoca_build` instead. Do not parse and reserialize canonical DAE
through JavaScript Number: source IDs can exceed its exact integer range. Keep
the original JSON payload bytes when passing them back into bindings. The existing
`response.pretty` string is canonical DAE JSON generated in Rust and can be
passed directly to `render_target` without parsing and reserializing the inner
JSON; it already preserves integer source IDs.

The existing public binding is `render_target(dae_json, model_name, target,
manifest_source, templates_json)`. It currently admits DAE/AlgorithmCode targets,
not arbitrary Solve targets. `prepare_gpu_simulation` remains an explicit-ODE
profile and correctly rejects stateless residual-projection models such as
CvMap and the current Harris source. A stateless GPU refresh target needs an
issued causal schedule and dispatch dependency contract; this change does not
invent one or disguise those models as ODEs.

## Scope and review risks

Checked specifications: SPEC_0007 (IR stage/wire contracts), SPEC_0032 (compact
families and derived scalar views), SPEC_0029/0041 (codegen and binding owners),
SPEC_0021 (complexity), SPEC_0033 (upstream-first verification), SPEC_0025 (review).
SPEC_0048 was consulted as a draft only. No MLS semantics changed.

Compact shape validation remains eager, including the existing malformed-stride
diagnostic. The main correctness risk is moving expansion-dependent scalarization
errors to the point that a scalar consumer requests them; native capability
validation remains in place. The new demand views cache
successes and errors and expose no public API. The wire fix preserves the strict
DAE schema rather than teaching consumers to strip arbitrary fields.

Verification results and remaining release gates will be recorded below after
commands finish. No browser, GPU hardware, full MSL cohort, package publication,
PR, push, or merge is implied by these backend tests.

Focused validation passed: `cargo test -p rumoca-phase-codegen -p rumoca-bind-wasm
--quiet` ran 62 binding unit tests, 249 codegen unit tests, 54 codegen integration
tests, and one active codegen doctest. One existing doctest is intentionally
ignored. Numerical assertions were not weakened to admit malformed IR: the first broad run identified diagnostic timing drift, and the final code
retains eager compact validation without scalar expansion.

The first fixed-20 canary invocation used the default shell, retained all original
budgets and two stage/simulation workers, and exited with failure because `omc`
was absent. Its report is `target/msl/slam-cv-canary/msl_quality_current.json`:
20 selected, 11 compiled/balanced, 9 simulated to completion, 2 solver failures,
9 typed ToDae refusals. `parity_measured` is false; this is not a pass or a parity
number. The missing comparator is recorded rather than bypassed with
`--allow-unmeasured-parity`. The pinned `.#modelica` shell supplies OpenModelica
(`omc --version` reports `a96aa1a-cmake`) for a subsequent explicitly recorded
measurement after the compact CPU/WASM work freezes.

At this focused checkpoint, workspace formatting has been applied and
`git diff --check` passes. Whole-workspace clippy, tests, documentation, and a
measured fixed-canary delta remain pending until the next isolated kernel change
freezes. These commits are review checkpoints, not a merge/release assertion.
A full MSL cohort and a separate WASM package rebuild were intentionally not run
under the parent's two-CPU resource budget; neither is claimed here.

Focused implementation commits:

- `75dec420a`: demand-built scalar fallback views and native image-family tests.
- `b15673ab0`: canonical compile payloads and compile-wire roundtrip test.

Code budget for those two commits (documentation excluded):

| Metric | Lines/items |
| --- | ---: |
| Production lines added | 142 |
| Production lines deleted | 85 |
| Test lines added | 227 |
| Test lines deleted | 6 |
| Public items added/removed | 0 / 0 |
| Files touched | 7 |
| Net lines added | 278 |

The 57-line production net growth implements one shared, error-preserving lazy
projection and its two established template views. The compression pass removes
the eagerly populated legacy wrapper and duplicated linear-solve inventory
queries, retaining the IR owner's existing compact query. The larger test delta
covers image-sized extent independence, explicit lazy state, canonical fallback
parity/allocation reuse, error propagation, and strict DAE wire roundtrip.
