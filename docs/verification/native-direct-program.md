# Single-call native assignment programs

This composes an existing construction-issued native assignment schedule into
one portable CPU WASM module. It preserves the `native-direct-assignments-f64-v1`
stage API and adds `prepare_native_program(source, model_name)`, using the same
source compilation, Solve lowering and checked stateless admission.

The backend consumes the owner's exact value kernels and stage order. It does
not isolate residual equations or discover a schedule. Existing scalar and
affine emitters retain each complete source prefix and its arithmetic order;
their terminal stores write directly to the issued Y ranges. Independent
per-point assignment and disjoint target coverage are established by the
schedule's owner. No host stage calls, output copies, scratch buffer or compiler
WASM memory are needed for execution or artifact emission.

## Artifact and ABI

The new profile is `native-direct-program-f64-v1`. Its JSON contains model name,
source SHA256, compiler version/revision, Solve schema version, module bytes and
SHA256, VarLayout, input names, parameter defaults, and the issued schedule's
source-node/target-range metadata. The latter is provenance, not an execution
plan for a host. The profile limits both memory and module bytes to 64 MiB.

The module imports unshared wasm32 `env.memory` and the existing standard math
intrinsics required by its retained programs. It exports `memory` and:

```text
eval_assignments(yPtr:i32, pPtr:i32, time:f64,
                 reservedSeedPtr:i32, reservedOutputPtr:i32) -> void
```

The artifact uses Y at byte zero and P immediately after `y_count * 8` bytes.
There is no seed/output storage. Hosts write P and call once with the last two
arguments zero; every output is already in its named Y storage. The two reserved
arguments are ignored. Before any write, generated guards require aligned,
in-memory, nonoverlapping Y/P buffers, using widened pointer-end arithmetic.
The bytes-only backend API is
`compile_native_assignment_schedule_wasm_bytes(schedule, layout)`.

## Verified execution boundaries

Three focused Wasmi tests pass. Compact mixed ScalarPrograms/Map and neighboring
AffineStencil fixtures at 16, 14,400 and 57,600 elements execute eight changed
input frames. Every one of their 2N outputs is finite and bitwise identical to
both independent expressions and the existing issued-module/copy ABI. Each
fused binary is asserted below 4 KiB; image domains become bounded runtime loops,
not an image-sized emitted instruction stream. Reserved invalid pointers do not
touch scratch storage or P. An instrumented unused `sin` verifies complete-prefix
execution before target writes. Separate alias, partial-overlap, alignment and
out-of-bounds controls trap without changing any memory; mismatched layout
cardinalities are refused at emission.

These are execution-emitter controls built from checked Solve fixtures, not
whole image source-compilation or detector-equivalence claims. The test command
is:

```text
cargo test -p rumoca-exec-wasm --test native_program -- --nocapture
```

The final warm run passes all three tests in 0.81 seconds. The first two compile
attempts exposed and repaired visibility/type qualification and a test field
spelling; their terminal failure records are retained. Strict backend Clippy
passes all targets/features in 18.74 seconds:

```text
cargo clippy -p rumoca-exec-wasm --all-targets --all-features -- -D warnings
```

Both commands use two jobs/threads on cores 6/7 with nice 10 and no incremental
compilation. The separate cache is capped at 2 GiB; aggregate process RSS is
capped at 8 GiB, with command deadlines and a 16 GiB available-memory floor.
The cache after Clippy is about 1.51 GB; observed peak RSS remains below 1.1 GiB.
Durable logs and actual terminal/resource reports are under
`/tmp/rumoca-fusion-exec-check-20261004`. Source formatting and diff checks pass.

The actual Modelica binding tests also pass on the integrated base
`b1ebcc2e6c4bb7dd3bf6809cbd53fe7650b1c060`. Three new tests compile the small
RGB/gray/gain/score mixed model, compare every output against v1 through eight
changed input frames and an edited constant, and check source/module/provenance
identity. Time, empty P, a sin-to-exp dependency and explicit state/coupled
refusals are exercised through one published Wasmi entrypoint. All 13 tests
selected by the existing `native_` filter pass, retaining the old stage API,
wire reissuance, input defaults and compiler/source-root controls.

```text
cargo test -p rumoca-bind-wasm --no-default-features --features native-assignments \
  native_mixed_assignment_tests::program -- --nocapture
cargo test -p rumoca-bind-wasm --no-default-features --features native-assignments \
  native_ -- --nocapture
cargo clippy -p rumoca-bind-wasm --no-default-features --features native-assignments \
  --all-targets -- -D warnings
```

The first cold binding build reached its 600-second deadline before running any
tests; its exit 124 is retained and is not a numerical pass. The combined-tree
warm retry compiled in 3m21s and passed all three tests in 0.03s. The subsequent
13-test run passed in 0.07s after 23.58s of compilation. Strict scoped binding
Clippy passed in 1m58s. Across those gates the private cache stayed below 2.6 GB
and peak aggregate RSS below 1.7 GB. Commands retain two jobs/threads, cores 6/7,
nice 10, an 8 GiB RSS ceiling, a 7.5 GiB private-cache ceiling and explicit
deadlines. The shared cache and completed metadata cache were unchanged.
Durable terminal/resource reports and logs are under
`/tmp/rumoca-fusion-binding-check-20261004`; all 11 fusion Rust source hashes
match the earlier emitter proof. Pinned source formatting and diff checks pass.

The base includes the verified compact reverse-invalidation consumer and exact
fold projection graph. The following actual-WASM source gates are additional
evidence; they do not replace complete browser/runtime or upstream release gates.

## Fresh package and full-image source execution

The review-only full-web package was built from
`943aa7d7b745eccc63ceb0f6a8f678f1542b60c7`, with compiler version 0.10.1 and
reported revision `943aa7d7b745`. Its 35,338,588-byte compiler WASM has SHA-256
`a2310458a4723af3c43d1934b50d8ad7509e247f58465b2c1beb7e3b0eb5f135`.
The exact build command, source-tree identity, per-file hashes and resource report
are in `/tmp/rumoca-slam-native-full-web-943aa7d7b/build-proof.json`. Package
source/module/revision sanity, changed-input/source-edit canonical sign tests
(including signed zero, NaN and infinities), and ordinary stateless/stateful
input-event replay all pass. The production npm pin was not changed.

The application's durable record
`slam_web/dev/modelica-fusion-eigen6-verification.json` reports a full 160x90
Modelica source with 43,200 RGB inputs and 28,803 Y outputs. Its source SHA-256 is
`20ab5b24ec9f0321d0d3b24cab40196cb794ccca1cc532a0e1787242b559138b`.
Five issued stages have target counts 1, 1, 1, 14,400 and 14,400; they execute
inside one compiler-issued module through one published entrypoint. All Y values
match v1 bitwise across eight changing frames. Independent gray/score expressions
check another 230,400 values. Source edits, reset, JSON persistence, private
artifact copies, metadata isolation and negative admission checks also pass.
The generic consumer verifies the module SHA-256, but this durable report does
not capture that module digest; no full-program module hash is asserted here.

The first source gate failed only its final overlap-refusal fixture: changing
just the start of a differently sized span produced a valid earlier out-of-bounds
refusal. The fixture now duplicates a complete in-bounds span, retaining the same
expected overlap assertion. The corrected complete gate passes in 20.681 seconds,
with peak aggregate RSS 1,323,456 KiB, under its 180-second/8-GiB limits on cores
4/5. Both attempts are recorded in the application record; final log:
`/tmp/slam-native-program-943aa7d7b-final.log`.

These are actual compiler/kernel WASM executions under Node, not a complete
browser graph or runtime integration. Gate elapsed includes preparation and
assertions; it is not standalone per-frame throughput. Combined architecture,
workspace and fixed MSL canary gates remain pending. Production pin and runtime
activation remain unchanged, and a full SLAM program is still incomplete.

## Remaining capabilities

Fusion does not extend the owner to states, initialization, events, clocks,
general assigned/computed matrix products, FunctionFold/runtime control, dynamic
array storage or arbitrary coupled equations. Those remain explicit refusals.
One compiled stateless scalar/array program is a composition step toward the
connected Modelica pipeline, not proof of a complete vision/SLAM executable.
