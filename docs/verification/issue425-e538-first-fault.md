# Original Euler38 constructor-memory first fault

The actual retained `pr427-original-euler38-after-e538-20261011` run used source
e5388bd4bf2c08e4c20d29751bd3fedb95cb8fb6. The Linux two-GiB address-space test
executed exactly once and failed: the original benchmark child aborted with
SIGABRT, `memory allocation of 4456464 bytes failed`. The parent test reported
0PASS/1FAIL/0ignore in234.32 seconds after compilation. The outer guardian exited
101 after475.803 seconds, with peak process-groupRSS2,947,784KiB and no resource
stop. GroupRSS includes compilation and the parent; it is not the child's
peakRSS. The child limit was RLIMIT_AS2097152KiB.

The retained core entry for PID1930264 identifies the exact original CLI command
and model. Its stored symbol trace reaches:

1. Rust allocation failure inside `hashbrown::RawTableInner`, called by
   `regalloc2::ion::moves::apply_allocations_and_insert_moves`.
2. Cranelift's `compile_function` and JIT `define_function_with_control_plane`.
3. `CraneliftEmitter::compile_projection_application`,
   `ProjectionBatch::compile_all`, `CompiledProjectionJacobian::new_all`.
4. Native `prepare_projections`, checked runtime projection preparation,
   `SolveRuntime::construct`, retained FMI instantiation and the original
   `RkLikeHotBench::build`.

This is backend construction heap exhaustion. It differs from the original
Latent512 anonymous-JIT execution stack overflow. The caller-owned heap-tape
repair does not establish an Euler38 memory improvement. It may change generated
code/register pressure, but that effect remains unmeasured. The stored core
reports a missing executable build-id; no current rebuilt binary was paired with
it for an offline debugger, and no specific block/color/instruction count is
claimed from the symbol trace alone.

Source comparison from e538 to224fbf99490e0154aae3aca74034bb35540cf48a leaves
`ProjectionJacobianApplication`, shared ComputeBlock scalar projection and
runtime prepared-projection selection unchanged. `projection_jacobian.rs` is
unchanged. `projection_batch.rs` changes only the heap arena ABI/acquisition.
The original model contains no div/mod quotient family, so the new quotient
activation repair is not direct evidence for this failure.

The first potential compact-to-scalar growth site is the existing shared
`to_scalar_program_projection` fallback, reached through
`projection_jacobian_source`: a sole scalar owner is shared, while tensor/Map
nodes produce checked scalar programs. This source path is not proof that the
failing Euler application actually required that fallback. The definite
native-emission growth site is `compile_projection_application`: it emits every
selected program for every issued color into one function before one
`define_function`. The immutable source is Arc-shared, not deep-cloned per
application. Invariant operations reuse retained SSA values across colors, but
other operations are emitted per selection, and shared load/retained values can
span colors. The core proves allocation failure in register allocation for this
function; it does not measure the proportion due to scalarization, repeated
emission or live-range pressure. No performance repair was implemented here.

The unchanged original30 fixture SHA256 remains
`dccdafed12d656db2c6758b6e0e5f7ca5d85accb89847d6715c4c6f5f5c4b89d`.
Changing only authored nx/ny30 to38 yields SHA256
`1bd65c874537a23acd43cde3d5d1879d84bf1b1c2affa685df1646d84e8c6a5d`.
Model name stays `E30x30`. Existing ordinary Linux workspace CI includes the
two-GiB benchmark test; the independent quiescent/native-receipt test is also
registered in suite_core. No new test owner or model is needed. The smallest
current-source qualification uses default CLI features, sequentially:

```bash
cargo test --locked -p rumoca --test suite_core process_allocator_address_space::issue_425_original_38_bench_fits_a_two_gib_address_space -- --exact --nocapture
cargo test --locked -p rumoca --test suite_core reported_bug_runtime::issue_425_original_38_preserves_quiescent_states_and_native_receipts -- --exact --nocapture
```

Each selects one test. The original benchmark remains rk-like,t_end0.1,dt0.01,
one iteration,zero warmups,RAYON_NUM_THREADS1 and RLIMIT_AS2GiB. The second test
checks all5776 original state values at all11 authored samples and native receipts;
it is not a memory measurement. Both current after outcomes remain unrun here.

Retained evidence SHA256:

| File | SHA256 |
|---|---|
| original-euler38-two-gib-1.log | b421e3687f1ceddce1db2b4f93a8085a534b41bd58048d5a98aa3e1ee3948bf1 |
| original-euler38-two-gib-1.guardian.json | 198bca52177f641288b3170cb3c5a3477bb2aacde54900365e3de279fe3933b8 |
| coredump-info.log, source-review snapshot for PID1930264 | f6410989e468d464e3a3393b5bd33411f18cafb6f09b516e84516478667239a1 |

The core-info snapshot is retained under the owned profiles attempt
`euler38-e538-original-core-source-review-20261011`. No model rerun, core dump,
Cargo invocation, current target mutation or issue closure occurred in this audit.
