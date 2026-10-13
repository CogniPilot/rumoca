# Source-ordered assertion qualification

The unchanged [AssertionFaultOrder.mo](fmi-ls-typed-accessors/AssertionFaultOrder.mo)
contains two fatal assertions before a dynamic one-element array access.
The repaired source producer emits a checked assertion at each authored
statement. The canonical evaluator and shared FMI C kernel stop in the same
invocation, before later statements or loop iterations. Failed-only message
captures retain their actual typed source values. Ordinary result tuples are
published only after successful completion; a stopped invocation supplies no
ordinary or tangent placeholders.

The C25 observation opcode projects only the assertion predicates of that
checked invocation. Its immutable binding includes the exact table, operation,
owner, ordered actions and source assertion-flow proof. Its dependency
certificate includes every actual input, including later numerical failure
paths. A private collector seals the evaluator's exact borrowed program
inventory. Generic row contexts and foreign replacement program allocations
cannot execute the observation. Nested normal diagnostic outputs retain only
canonical inactive cells; actual observations remain in the private carrier.
Schema 76 rejects earlier wire versions.

## Executed controls

The public compiler/export API renders the experimental FMI-LS target; strict
Clang builds all shared C translation units both natively and for
`wasm32-wasip2`. The component validates with wasm-tools and executes through
the pinned standard WIT interface in Wasmtime. Both hosts receive the standard
FMI logger callback, preserving the C instance's context for its entire
lifetime. No private diagnostic API is used.

| valid | index | Initialization | Standard assertion callback | Float64 output query |
|---|---:|---|---|---|
| false | 1 | Error | exactly `first authored assertion` | Error; native sentinel unchanged |
| false | 2 | Error | exactly `first authored assertion` | Error; native sentinel unchanged |
| true | 1 | OK | none | OK, value 1 |
| true | 2 | Error | none | Error; native sentinel unchanged |

Queries after failure do not replay the diagnostic. Every case resets the same
instance, and the hosts free it after the final case. A separate
[NestedLoopAssertionFault.mo](../../crates/rumoca/tests/fixtures/NestedLoopAssertionFault.mo)
control exercises child forwarding and a compact three-iteration loop; it
reports exactly `nested first 1`, with no later assertion or bounds message.
Both unchanged controls pass in native FMI C and the compiled WASI component:
2 tests passed, 0 failed, 20.93 seconds (43.40 seconds compiler/test build).
The final compiled control uses integration base `6b15337c0` plus the
coherent schema-76 cutover.
The historical baseline has no authored diagnostic for false/index 2, as
recorded in [the earlier receipt](fmi-ls-typed-accessors.md).

Additional strict-C controls preserve the exact Integer capture
9007199254740993, stop before bounds failure, retain ordinary output sentinels,
and discard a warning batch when a later operation fails. Successful warning
publication occurs once. The reference evaluator covers numerical-fault
ordering, initialized captures, nested forwarding, foreign programs, severity
and selector forgery. AD preserves exact Boolean/Integer primals and issues
zero tangents only under the existing Real-input zero proof; nonzero Real
inputs still require an admitted directional relation.

## Reproduction

Use owned scratch storage and four build/test workers:

```sh
export CARGO_TARGET_DIR="$HOME/scratch/rumoca/ir-check/target"
export TMPDIR="$HOME/scratch/rumoca/fmi-slam-codegen/tmp"
export CARGO_BUILD_JOBS=4 RUST_TEST_THREADS=4 RAYON_NUM_THREADS=4
nix develop .#ci-template-wasm --command cargo test -p rumoca \
  --features template-runtime-tests --test suite_template_runtime \
  fmi_ls_wasm_preserves_ -- --nocapture
```

A successful assertion initializes its diagnostic captures through the canonical
`SolveValue::inactive_assertion_message` owner. The earlier C template wrote
zero, which violated Integer domains excluding zero. A strict-C control fails
on that baseline and passes after the repair, checking the complete ordinary,
predicate and capture tuple for both `[2, 7]` (inactive value 2) and `[-9, -3]`
(inactive value -3). The renderer reuses the existing constant arithmetic
emitter; it adds no domain policy. The affected Codegen suite passes 267 tests,
and final all-target Codegen/public compiler clippy passes with warnings denied
(45.60 seconds).

Detailed task-owned logs remain under
`$HOME/scratch/rumoca/fmi-slam-codegen/evidence/`:
`fmi-native-wasi-first-fault-inactive-final.log`,
`fmi-inactive-domain-before.log`, `fmi-inactive-domain-after.log`,
`fmi-inactive-domain-codegen-suite.log`, `fmi-inactive-domain-clippy.log`,
`assertion-cutover-broad-flow.log`
and `assertion-cutover-clippy-final.log`. The broad pre-integration control passes
619 Solve IR, 203 Solve phase, 320 Eval and 266 Codegen unit tests. The schema-76 integrated control against `17f2c757e` passes 627 IR,
205 Solve phase, 320 Eval, 266 Codegen and 593 Solver unit tests. All-target
clippy passes with warnings denied for these crates, both native backends
and `rumoca` with template-runtime-tests (26.03 seconds). The later reviewed
read-only analysis-cache repair is integrated as base `6b15337c0`; its
IR/Eval rerun passes 627/325 tests and is retained in
`assertion-cutover-cache-integration.log`. The compact durable
[receipt](fmi-ls-first-fault/receipt.json) records the control source hashes
and exact qualification boundaries.

## Qualification limits

This qualifies the source-order/first-fault controls and shared C carrier.
It does not qualify the full SLAM source, a browser host, 10x throughput or a
standards-conformant complete FMU archive. The unchanged source's remaining
export resource failures are recorded in
[the integration receipt](slam-fmi-integration-20261008.md).
The experimental target still emits a component crate/C/WIT rather than a
complete FMU archive or matching model description. The test's separate FMI3
metadata supplies typed references and is not claimed as the component's
packaged metadata. Native Cranelift/raw WASM execution and directional
captured/forwarded assertion-stop adapters retain explicit refusals; the
[uncaptured directional repair](nocaptures-directional.md) preserves checked
primal conditions, authored order and diagnostic identity. The compiled FMI-LS target
uses the qualified shared C kernel. Unsupported message effects or forwarding
remain construction refusals. The complete source transaction tuple retains
atomic publication and exposes ordinary invocation failure.
