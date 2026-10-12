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

`compiler-source.sha256` follows the original `git ls-files -z` order. Each
line is `sha256`, two spaces, the tracked path, and a newline. Regular entries
hash their raw file bytes; symbolic links hash their raw target bytes without
following the link, including links to directories. Other entry types fail
before the build. `issuance.json.sourceManifestFormat` records this exact
format, and the replacement verifier requires it and recomputes identical
bytes with `lstat` and `os.fsencode(os.readlink(...))`. Git revision, tracked
order, compiler binary and authored model identities remain independently
checked. Historical receipts retain their original format and are not reused
as current issuance receipts.

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
The example's four positional arguments remain `SOURCE MODEL VARIANT OUTPUT_JSON`.
Its discoverable clap interface additionally requires `--source-sha256`,
`--compiler-revision`, `--compiler-source-sha256`, and
`--producer-binary-sha256`. The wrapper passes those actual measured values as
individual argv entries. No metadata environment variables or default identities
are read; `--help` describes each required field. Source filenames remain
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

The metadata CLI followup is based on
`b4e38503723816d0529168c46ac1b027ca1ea73d`. Its
[tracked-source environment audit](../pr427-workshop-ci/issuer-cli-env-audit.json)
scans all 3,035 tracked first-party Rust/TS/JS source files using the existing
registry's extension, directory and sentinel rules. It finds no configuration
literal offenders after replacing the four issuer reads. Nine architecture
banlist literals remain excluded by the existing test policy. This bounded
source audit does not inspect untracked files or replace execution of
`test_rumoca_env_vars_are_registered`; discoverable CLI parsing and actual
source issuance still require qualification on the new source.
