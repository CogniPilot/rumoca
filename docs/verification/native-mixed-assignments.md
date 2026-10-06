# Mixed scalar and compact native assignment stages

This extends the construction-issued `native-direct-assignments-f64-v1`
stateless profile to canonical `ScalarPrograms` alongside existing `Map` and
`AffineStencil` nodes. No consumer ABI or Solve wire schema changes are needed.
The browser still executes issued stages and copies their outputs to the issued
Y ranges; it does not isolate equations or discover dependencies.

## Admission and replay evidence

A scalar program must have exactly one terminal `StoreOutput`, an owned bounded
Y target, bounded Y/P loads, and supported pure scalar operations throughout its
complete source prefix. The existing exact target-assignment owner must prove
`Direct` or `Zero`. The same owner materializes the value without removing any
earlier operation. Each scalar stage retains the original source node, program,
logical output slot, output store, source span, and prefix. Replay compares Const
bits, not floating-point equality. Sparse explicit scalar output indices and
local contiguous indices use the canonical source-output owner.

The existing scheduler requires complete nonoverlapping source outputs and Y
targets, derives dependencies from the retained loads, and refuses cycles. It
can issue several scalar stages from one canonical source node. Scalar stages
use `target_count = 1`; compact families retain their original domains.
`Zero` tensor boundaries now use the same exact shared materializer as scalar
zero rows. Certificates remain private derived sidecars, omitted from wire and
reissued from the complete source on canonical decoding.

## Focused checks

The new owner controls exercise compact mixed inventories at 16, 14,400, and
57,600 elements, reversed sparse scalar slots, source order differing from
dependency order, complete-prefix retention, signed-zero/span/mapping changes,
cycles, aliases, address bounds, unsupported prefix effects, nonlinear target
coupling, nonterminal output stores, and multiple-output programs.

The new binding controls compile an editable Modelica RGB-to-gray-to-score
pipeline with scalar gain and boundary equations. Wasmi executes the actual
issued modules across eight changing-input frames with NaN-poisoned Y memory;
every scalar and array output is compared bitwise with independent expressions.
An edited source must change its source and module identities and its numerical
outputs. Wire decoding must reissue the same source-bound mixed schedule.

The previous review package (`94cff417c`) rejects the identical mixed Modelica
fixture because its native owner does not admit the scalar node family. The
before source digest is
`348b9855bc6b31420f271c8161f9e6d7339d1d111b5caa176a3d4be9c075fd2a`;
the refusal is retained in `/tmp/rumoca-mixed-native-before.json`.

Source formatting and diff checks pass. The scoped owner gate passes all 14
native-assignment controls (seven new mixed controls), with a 70-second warm
build and subsecond test execution. It ran with the application's pinned Rust
1.95 CI shell, two jobs/threads, `CARGO_INCREMENTAL=0`, nice 10, cores 6/7,
and the shared bounded RAM cache. Command:

```text
cargo test -p rumoca-ir-solve native_assignment
```

The first native binding build was stopped before the shared cache exceeded
10 GiB, with exit 143 and 10,711,916,544 allocated bytes at release. Its tests
had not started; there was no observed source error. The resumed binding gate
passes all 10 selected tests, including all three new mixed/scalar controls,
after a 366-second dependency build and 50-millisecond test execution:

```text
cargo test -p rumoca-bind-wasm --no-default-features --features native-assignments native_
```

Scoped Clippy was then stopped before checking by the cache cap; it is not a
pass or a source diagnostic. A racing temporary-file `du` error had terminated
the first step's watcher, allowing the cache to grow past the agreed 10 GiB
limit before the second step stopped at 10,933,977,088 allocated bytes. The
watcher now tolerates that expected race and fails closed when its metric is
unavailable; future gates also cap aggregate process-group RSS at 8 GiB.
The completed test executables and their split DWARF were retired first. An
additional 65,806,128 bytes of explicitly authorized completed binding-context
IR/core libraries and their split DWARF were then retired, after matching their
exact fingerprint identities to that completed binding. Their file hashes,
timestamps, and raw fingerprint evidence are retained in the retirement
manifest; Cargo can regenerate them. This brings allocated cache storage to
10,737,332,224 bytes, just under the cap. Other agents' artifacts were preserved.
Durable logs, retirement manifests, and explicit terminal/resource records are
under `/tmp/rumoca-mixed-native-checks`.

Strict scoped Clippy subsequently passes for both changed packages, all targets,
with only the native-assignment feature enabled. The approved metadata-only
cache was separate, capped at 2 GiB with an 8 GiB aggregate-RSS limit and a
600-second deadline. Command:

```text
cargo clippy -p rumoca-ir-solve -p rumoca-bind-wasm --no-default-features --features native-assignments --all-targets -- -D warnings
```

Its fresh dependency check took 337 seconds; the final allocated cache is
992,985,088 bytes. Pre/post hashes of every changed Rust source, including the
new untracked modules, match. The unique log and resource/exit report are under
`/tmp/rumoca-mixed-native-clippy-20261004T051209Z-2463532`.

Fresh browser-package acceptance and combined upstream gates remain pending.
No after-WASM performance or production integration is claimed from the native
owner/execution checks alone.

## Remaining boundaries

This does not admit general affine/nonlinear equation solving, multi-output
scalar programs, seeds, external/effectful operations, dynamic indexing,
FunctionFold, computed matrix products, persistent states, initialization,
events, clocks, or external tables. An expression-only MatMul WASM emitter is
not a certificate for a complete assigned matrix pipeline. Full Modelica vision
and estimator integration remain separate acceptance work.

## Whole connected-program composition

This is one artifact and one shared memory with several certified kernel
modules. The current host still invokes each stage and performs its issued
output-to-Y copy; it is not yet one compiled module with one per-frame call.
Modular Modelica source and one whole-program executable are compatible goals.
An owning compiler emitter can next consume this certified schedule, emit the
checked value kernels and exact output-to-Y commits inside one exported
function, and prove its bounded shared scratch/layout/import contract. Such
composition must use the already issued dependencies and assignments, rather
than deriving a new schedule in a browser or treating residuals as values.
Single-module composition does not itself solve the remaining native function,
computed-matrix, state/control, or connected-model interface admission gaps.
