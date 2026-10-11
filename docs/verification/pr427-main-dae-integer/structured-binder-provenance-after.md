# Structured source binder authority

`99d8d3a8a` carries the existing Core `IndexBinderProvenance` into each
`StructuredIndexBinder`. `b25f807e1` requires exact source declaration
equality at B1c source-family admission. This closes 817's foreign LHS gap;
the earlier geometry/order/target/recurrence proofs remain required.
The immutable before witness is `d62ecd954` and remains unqualified.

Source authority comes from the existing Resolve-issued `ForIndex.binder` /
`ComprehensionIndex.binder`. The eight source producers are Flatten's compact
equation domain, Instantiate's regular connection domain, and ToDAE's function
loop, two comprehension, and three model-algorithm domain constructors.
Compiler-created projection, slice, connection-range, vectorization and seed
axes explicitly carry `Generated`. Lifting a child domain changes its axis
ordinal and retains its provenance. No source IDs are derived from names.
The B1c shortcut declines generated axes: the marker itself does not issue a
generated declaration identity. The existing packed path remains available.

This follows SPEC_0001's identity domains/semantic keys, SPEC_0029 and its
SPEC_0041 iterator-provenance owner, and SPEC_0032's domain/scalar-view
contracts. MLS §§8.3.3 and 10.6.1 define iterator scope. No new provenance
vocabulary, evaluator, sidecar, owner-equality relaxation, or scalar expansion
is introduced. The constructor migration is 184 existing literals across
109 Rust files, plus the Core field. Most literals are geometry-only fixtures.
Original authored public Modelica models and their assertions are unchanged.

The mandatory field has **no serde default**. Missing provenance rejects wire
decoding; the existing enum rejects `Source(0)`. DAE schema 44 becomes 45;
Solve schema 74 becomes 75 and its representative golden version changes.
Existing parsed-artifact/source-root caches contain `StoredDefinition` or
source text, not constructed domains, so their schema stays unchanged.
Domain equality now also retains provenance rather than conflating a source
declaration with a geometry-identical generated axis. This is a Rust public
field and wire change; no executable function or memory ABI is added.

All qualification below is **UNRUN**. Formatting and `git diff --check` pass.
Root owns the guarded build lane. Narrow expected selections:

```sh
cargo test --locked --offline -p rumoca-core --lib structured_binder_wire_
# 2: JSON/binary source/generated roundtrip; missing/zero-source refusal
cargo test --locked --offline -p rumoca-phase-flatten --lib provenance_tests
# 2: source retention/lift; absent refusal/generated retention
cargo test --locked --offline -p rumoca-phase-dae --lib b1c_source_templates
# 6: original coverage/order/scope/recurrence controls plus foreign LHS/generated authority
cargo test --locked --offline -p rumoca --test suite_core --features fmu-packaging main_dae_integer_quotients_keep_issued_outer_and_owned_comprehension_binders
# 1 unchanged original public outer fixture, both Interpreter/Auto, n=0/1, full 11 samples
cargo test --locked --offline -p rumoca --test suite_core --features fmu-packaging main_dae_integer_quotients_owned_comprehension_executes_independently
# 1 independent unchanged owned fixture, both Interpreter/Auto, n=0/1, full 11 samples
```

The all-target source check and current-version DAE/Solve replay tests also
remain required. This document does not claim forced native execution, full
workspace qualification, CI green, combined PR421 qualification, or SLAM.

## Source-issued WASM fixture refresh dependency

Six current historical table artifacts lack the new mandatory binder field.
Their producer revisions/hashes must not be relabeled or hand-edited. The
existing external source producer is
`$HOME/git/slam_web/dev/rumoca-native-source-producer.rs`; its CLI is
`SOURCE MODEL OUTPUT_DIRECTORY`. It compiles through `Session` and
`lower_solve_package`, and requires `RUMOCA_PRODUCER_REVISION` and
`RUMOCA_PRODUCER_SOURCE_SHA256` from the actual frozen source. Root must build
it against the corrected isolated tree and bind its actual binary digest.
The existing test wrapper additionally requires `producerBinarySha256`, which
the source producer itself does not write; the issuing harness must record
that fact from the executable, along with actual source/table/artifact hashes.

The unchanged sources under `crates/rumoca-exec-wasm/tests/suite_typed_calls`
and exact output roster are:

| Source / model | Producer variants | Current fixture destinations |
|---|---|---|
| `OrderedFrameFold.mo` / `NativeOrderedFrameFold` | baseline, gain-two, subtract | `fixed_tables/{baseline,gain-two,subtract}.json` |
| `IntegerSeedFold.mo` / `NativeIntegerSeedFold` | baseline | `fixed_integer_tables/baseline.json` |
| `OrderedTensorFrameFold.mo` / `NativeOrderedTensorFrameFold` | baseline, gain-two | `tensor_tables/{baseline,gain-two}.json` |

The producer's existing variants replace `gain = 1.0` with `gain = 2.0` and
`total + gain*values` with `total - gain*values`. Baseline uses the exact
original source. Outputs must first go into owned scratch evidence, with the
source pin, binary/hash manifests, elapsed measurements and issued table,
then be copied to fixture locations after actual current-version replay and
executor checks. Any extra emitted variant remains review evidence until it
has a declared test consumer. Preserve the four existing historical-refusal
artifacts under `source_tables` and `integer_tables`, and archive the previous
six issued artifacts before replacement. The fixture wrapper's recorded
revision/source/binary expectations must follow actual newly issued facts.
Regeneration and executor qualification remain pending, separate from this
source migration.
