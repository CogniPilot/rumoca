# Canonical source-issued WASM fixture regeneration

The current CLI `compile --emit solve-json` cannot issue the fixture table:
`rumoca::CompilationResult::to_ir_json` calls `lower_solve_problem`, serializes
only `SolveProblem`, and discards `LoweredSolvePackage.pure_calls`. A custom
target's rendering context is not the raw checked table either.

`issue_wasm_source_table` is a small example using the existing
`Session::compile_model` and `lower_solve_package` entries. Its owning package
already has the required dev dependencies; no dependency or public API is
added. It replaces the external review producer for this refresh, leaving
`slam_web/dev/rumoca-native-source-producer.rs` untouched. The portable wrapper
is `crates/rumoca-exec-wasm/tests/suite_typed_calls/regenerate_source_tables.mjs`.

Under the root's exclusive guarded lane, with the standard Cargo/RSS/disk
limits and owned cache environment already set, run:

```sh
node "$fixture_tree/crates/rumoca-exec-wasm/tests/suite_typed_calls/regenerate_source_tables.mjs" \
  "$fixture_tree" "$HOME/scratch/rumoca/pr427-wasm-fixture-refresh-20261011/issued"
```

Here `fixture_tree` is the absolute path to the clean, committed corrected
tree including the helper. Its output parent exists and `issued` must be new.
The wrapper performs exactly one build:

```sh
cargo build --locked --offline -p rumoca --no-default-features --example issue_wasm_source_table
```

It then invokes the built example six times, with the exact source/model/variant
roster in [the migration record](structured-binder-provenance-after.md).
Each invocation is `SOURCE MODEL VARIANT OUTPUT_JSON`. Source filenames remain
the original `.mo` filename inside separate variant directories, preserving
the document name used for source provenance. Variant transformations exactly
match the existing test oracles and require one declared replacement site.
None of the original `.mo` files or tracked JSON fixtures is overwritten.

The wrapper hashes every tracked source file, records the actual Git revision,
build command/environment and compiled executable digest, and verifies that
source bytes, revision and executable remain unchanged through issuance.
`compiler-source.sha256` defines the source-manifest hash format.
`producer-build.log`, six exact sources, six producer logs, six `issued.json`
artifacts and `issuance.json` preserve the observed evidence. The example
records actual compile/lower elapsed time and current DAE/Solve schema values.
The root guardian provides the independent source seal, resource receipt and
toolchain binding; this helper does not replace those checks.

The six destination paths appear in `issuance.json`. The wrapper's JS never
parses/reencodes a typed table: Rust serializes its exact i64 payloads and JS
only hashes those bytes. The independent issuance manifest contains textual
hashes/paths/build facts, not reconstructed IR. Recorded fields satisfy the
fixture wrapper's source/revision/compiler-source/binary identity contract;
its pinned expectations must be updated to these actual new facts only after
successful issuance, current-version checked replay, and executor tests.

Archive the old six issued files first. Preserve the existing four historical
wire-refusal artifacts and every arithmetic/fault/atomicity/recovery assertion.
Generation alone does not prove execution. The actual `suite_typed_calls`
executor suite must consume the new fixture bytes and qualify its canonical
oracle plus Wasmi execution before replacement is reported complete.

```sh
cargo test --locked --offline -p rumoca-exec-wasm --test typed_calls
```

The source-fixture modules are `suite_typed_calls::source_tables` and
`suite_typed_calls::tensor_source`; the full target retains the independent
Integer, alias, fault, recursion and control-flow coverage as well.

No build, generation, replay or executor run has happened on this candidate.
Only formatting, Node syntax and diff checks are eligible for source-only
verification. This is a separate helper commit, not a fixture metadata refresh.
