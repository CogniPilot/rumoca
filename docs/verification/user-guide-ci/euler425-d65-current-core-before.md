# Original Euler38 native preparation: current failure

The unchanged original Euler fixture, with only its two declared mesh extents
changed from 30 to 38, still fails on source
`d65f1f659e1c5b8cf4f107b5ae901eb05b8006b1`. This is an actual current failure,
not an inference from the older e538 allocation message.

The mandatory `suite_core` test
`process_allocator_address_space::issue_425_original_38_bench_fits_a_two_gib_address_space`
executed exactly once: **0 passed, 1 failed**. Its CLI child used the native
`rk-like` path, t_end=0.1, dt=0.01, one iteration, zero warmups, and the
original 2 GiB address-space limit. The child aborted while allocating
4,456,464 bytes. The outer guardian exited 101 after 581.583 seconds without
a resource stop; its aggregate peak RSS (including Rust compilation) was
3,039,500 KiB. This is not a child-only peak or a completed simulation.

Systemd retained the actual child PID 2866846 core. An owned dump and a sealed
copy of the executable were inspected offline with GDB 17.2. The executable
SHA256 is `23737e61f445f3f3945514dd8b65c8be1d83cfc9006c83f582f61eed6e8e32a8`.
Systemd reports no executable build ID. The source executable's inode/stat and
streamed hash were checked before/after copying. A same-basename sealed hardlink
was used for the final GDB read; it reports no executable mismatch warning and
its symbol addresses agree with the systemd backtrace. The earlier differently
named copy's GDB warning remains retained separately.

The current first-fault chain is:

```
hashbrown RawTable allocation
  regalloc2::ion::Env::apply_allocations_and_insert_moves
  Cranelift Context::compile / JITModule::define_function
  CraneliftEmitter::compile_projection_application
  ProjectionModuleState::define / ProjectionBatch::compile_all
  CompiledProjectionJacobian::new_all
  CompiledJacobianV::prepare_projections
  CraneliftJacobianExpression::prepare_projections
  refresh_projection::prepare_projection_jacobians
  SolveRuntime::construct
  SolveMeKernel::instantiate / BackendSimulationSession
  RkLikeHotBench::build
```

This identifies eager native projection preparation during runtime construction.
It does not measure the number of compiled programs/colors or prove a replacement
fix. The heap-tape repair for the separate Latent512 stack overflow does not
close this constructor allocation failure.

Machine-local raw evidence is retained under `$HOME/scratch/rumoca`:

| Relative path | SHA256 |
|---|---|
| `slam-integration-cycle-20261010/pr427-original-euler38-memory-d65f1-20261011/recipe.json` | `2c941baa267cd7721fb59d3f70f95acd5d5da1c4c86ca98c0251ad6204cb2306` |
| same attempt: `original-euler38-memory-1.log` | `c20d688e5bdc45d64cf5a52a643219283f66af04c702d3e99c9a17554f5e2eb6` |
| same attempt: `original-euler38-memory-1.guardian.json` | `9e0f2cad158e9e6eb355c0385bae4b1fcc8f6b8405176e581989a0e06661387f` |
| `workshop427-docs/profiles/euler38-d65-original-core-20261011/core.2866846` | `e4afd7453d5692d7a9efff00bf3eaf1e59ee42b2504a1d3467f510499ffb427a` |
| same profile: `offline-gdb-same-basename.log` | `0d4e2275aac46496480e0eb2a7658e7bf3a41f900d60759656ff4ef5216fec40` |
| same profile: `coredump-info.log` | `f9c817b381123a0463d719f3716fc1e1cede18d19d910d4c114682688c927733` |
| same profile: `executable-receipt.json` | `8ab1586ce5a6e9fede99dfdbdf0598c71f6adf15473b22f1021920e60f0eb77a` |

The independent quiescent-state test and any repaired-source result have no
claim in this receipt.

## Independent state gate

The same d65 source's separate
`reported_bug_runtime::issue_425_original_38_preserves_quiescent_states_and_native_receipts`
gate timed out at 600.041 seconds (guardian exit 124, SIGKILL,
`stopped=timeout`). It produced no requested samples and no Rust test summary.
Its aggregate peak RSS was 2,292,968 KiB and peak cache bytes were
7,333,744,640. This is **unqualified**, not one passing or one completed failing
Rust test. It does not independently diagnose an allocation failure or establish
an after-fix curve. The memory gate above completed its Rust failure report;
the state gate did not complete.

Raw evidence under
`$HOME/scratch/rumoca/slam-integration-cycle-20261010/pr427-original-euler38-states-d65f1-20261011`:

| File | SHA256 |
|---|---|
| `recipe.json` | `6ef22776960d882b592a6160cd2fa60b2a8181795805880ba6f6f481ba01c72c` |
| `original-euler38-states-1.log` | `3c45b600d6d77e0296ac1026d984a572188e9e9f049c6b9950f42f297df6dd2d` |
| `original-euler38-states-1.guardian.json` | `a5fc2f2ed8bdd4626c1459aec13ceb2b33f5c9594879bbfe361c0d4b95c8b58c` |
