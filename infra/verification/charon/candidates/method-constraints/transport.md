# Checked cross-crate closure facts

Updated 2026-09-08T02:14Z. Retention accepted by Claude; unadopted candidate.
Predecessor: d376ab27. SPEC_0033 §§2a/4 governs the cut; SPEC_0037 §§2/6
(DRAFT) governs its verification trust boundary. No Rumoca IR or Modelica
semantics changed. This is translator validation, not a preservation theorem.

## First divergence and implementation

The predecessor refuses actual
`rumoca_phase_solve::scalar_constant_derivative_refinement::admit_dae_profile`
at foreign closure declarations: facts issued in dependency borrow checking
cannot be retrieved in the consuming crate. Local declaration construction
already works; weakening the caller's lifetimes would not fix this boundary.

The fact library now owns a `FactSession` around the complete compiler run.
Its early query observer is unchanged. `after_analysis` retains owned records
and rustc-issued output paths; only successful return from the complete driver
can publish. Pinned `rustc_interface::run_compiler` finishes diagnostics and
calls `abort_if_errors` after codegen/linking, before returning normally. The
late-link regression exercises that reliance, including a prior good sidecar.
No external callback receives the private pending-publication value.

The strict current-format sidecar binds schema, Rust compiler version, executing
driver image, normalized target specification, stable crate identity, crate
content hash, and the SHA-256 digest of finalized artifact bytes. The consumer
uses `tcx.used_crate_source`, not filename guesses or dependency-directory globs.
Missing, malformed, unsupported, and empty-record cases remain distinguishable.
Successful imports are checked once per crate and shared by opaque handles.

These hashes bind local compiler artifacts and detect mismatches; they are not
a cryptographic signature proving that a hostile writer did not forge a valid
payload. The collector/compiler and local artifact production remain trusted.
Nor does this transport prove the Rust-to-LLBC semantic preservation theorem.

Cargo's wrapper identity does not invalidate nonworkspace dependencies. The
launcher therefore resolves the user's target directory through Cargo metadata
and owns a compiler/collector/schema-keyed child directory. An explicit CLI
target directory replaces metadata's inherited target-directory environment
input; the build receives one authoritative `--target-dir`. The invocation key
also rejects a driver replacement before compiling another dependency.
No artificial Rust compilation flag is used as a cache fingerprint.

Linux identity opens `/proc/self/exe`, not the replaceable pathname returned by
`current_exe`. Other platforms explicitly refuse executing-image identity until
their equivalent is implemented. This is a visible adoption blocker, not a
pathname fallback. The library uses rustc's SHA-256 implementation and serde's
decoder; no hash or JSON parser was reimplemented.

## Retained source and focused commands

`transport.patch` applies after `constructor.patch`. `transport.nix` composes
those candidates and the source-filtered `closure-regions` library. Recipe
evaluation and reconstruction have been tested; a complete Nix package build
has not. No live pin was changed. The eight-file driver/CLI/test patch and all
fact-library source are retained under infra, not only in a build directory.

From the applied Charon crate, with pinned nightly 2026-08-18 and matching
rustc-dev runtime, four build/test workers, and `CARGO_PROFILE_DEV_DEBUG=0`:

```console
cargo test --offline -p closure-region-facts --lib
cargo test --offline --test closure_fact_transport --test closure_required_regions --test closure_capture_regions
cargo clippy --offline --all-targets -- -D warnings
cargo clippy --offline -p closure-region-facts --all-targets -- -D warnings -D clippy::too_many_lines -D clippy::excessive_nesting
cargo fmt --all --check
IN_CI=1 cargo test --offline --test ui -- --test-threads=4
```

The standalone collection/consumer driver bodies now build through
`closure-regions/diagnostics/Cargo.toml`, which owns their dependency linkage and
lockfile. Its commands are in the library README. No shell runner was added.
This package builds the current repository's collection driver, including its
constructor-checkpoint `binding_checks.rs` module; it is not a Cargo target in
the separate applied Charon crate. `forged-binding-rejected.rs` remains an
intentional compile-refusal fixture, not a successful build target.

The full source fixture lives in `cross-crate/`: a real excluded path dependency
and consuming workspace member, with named, higher-ranked, and captured closures.
Use the candidate `charon cargo --preset=aeneas --mir=optimized
--error-on-warnings --include=closure_contract_dep --sysroot=default`, an explicit
`--dest-file`, and Cargo `--manifest-path=<fixture>/Cargo.toml --target-dir=<owned>
-p closure-contract-root --lib`. Aeneas uses `-backend lean -checks -strict-joins
-sequential`; check its generated file with pinned Lean, `-j4 -M4096
-DwarningAsError=true`. No intermediate file is edited before translation.

## Executed evidence

Logs and disposable output: `/tmp/rumoca-cross-crate-facts-GBMOCE`.

Charon tests and lint ran in `/tmp/rumoca-charon-captures-5tXAoU`. Final source
reconstruction `/tmp/rumoca-transport-retain-final-S9SqzM` applies patch
`ec3ee259840ff9e3d333e14daf87d2c7b4463e9fe4ebd23ebc753446ca89157d`
to the retained predecessor, then copies the current library Cargo.toml/src.
The complete Charon src tree, Cargo.lock, transport test and library
Cargo.toml/src compare byte-identically (4141b9/0). This is not a claim that
every auxiliary file in those scratch directories agrees: the reconstruction
also retains predecessor diagnostics and old README copies. Those copies are
not package inputs. Diagnostic build/lint evidence instead uses the current
repository diagnostics Cargo manifest and its referenced repository sources.

| Claim | Evidence and limit |
|---|---|
| Actual two-crate Rust-to-Lean | VERIFIED: Charon2e4ce1/0, Aeneasfcd35e/0, Lean68bfb3/0. Three complete caller paths; no added opaque closure body |
| Imported predicates actually consumed | VERIFIED: test7cbdca/0 checks the directed relation and absent converse in all three imported callable contracts, before corruption and after restoration. Merely emitting a file is insufficient |
| Missing/stale/malformed refusal | VERIFIED: five-test suite covers missing file/declaration, unsupported record, each header identity field, artifact digest, duplicate records, invalid slots, malformed JSON, and restored success |
| Successful-compilation publication | VERIFIED: invalid Rust refuses; a failed link emits changed metadata but cannot overwrite an existing sidecar. Early and late Rust failures preserve exit class2 |
| Cargo cache ownership | VERIFIED: ordinary Cargo cache is populated first. Config-only, environment-over-config and CLI-over-both each run twice; dependency builds once per namespace and requested root reruns |
| Executing image identity | VERIFIED: copied test executable replaces its own path; the executing image digest remains unchanged. Parent requires the child's post-assertion marker. Restoring pathname hashing compiles and kills this unchanged test1bfa2b/101 |
| Local predecessor regressions | VERIFIED: capture identity and exact required-predicate tests remain green |
| Actual DAE extraction | VERIFIED: frozen Charonfbd48a/0,62s including fresh dependencies. Same root/includes as the predecessor failure |
| Actual DAE Aeneas | VERIFIED: complete Aeneasc2fb20/0,93.77s, with checks and strict joins. Warnings about library trait models and existing external definitions remain |
| Actual DAE generated Lean | VERIFIED refusal: standalone d5265e/1. Iterator map/sum and ExactSizeIterator len fields disagree with the Lean library; missing supplied F64 model causes additional cascades. This is not a successful proof replay |
| Complete Charon UI | VERIFIED: final2773c2/101,422 passed/18 failed/2 ignored. Same failure set as predecessor:16 unapproved snapshots and2 dependencies without collected facts (auxiliary fixture and alloc sysroot). No snapshot or exclusion changed |
| Packaging and lint | VERIFIED: source reconstruction and Nix recipe evaluation; Charon all-target strict warnings, fact-library complexity checks, diagnostic build/lint and formatting pass. Full package/native/OCaml gates are not claimed |
| Repository diagnostic linkage | VERIFIED: fresh locked build and strict all-target/complexity lint67062f/0 use the repository diagnostics package. All16 collection invocations (eight fixtures, editions2021/2024, two actual compiler sessions each) pass. Logs: `/tmp/rumoca-transport-diagnostic-matrix-7cNiGM` |

One initial DAE replay failed because a concurrent test build replaced the driver
between dependency and root compilation; the artifact identity gate caught the
different images. Frozen binary pairs were used for the subsequent complete
runs. This failure is retained, not counted as a new semantic diagnostic census.
The first cache implementation also lost CLI precedence to inherited environment;
the executable three-cell test caught it before the corrected version passed.

## Review and remaining scope

Claude adversarially identified the executing-path race and a vacuous child-test
selection; both are fixed with executable controls. It also challenged cache
ownership and post-analysis failures. Their regression tests now include those
counterexamples. Its proposed redundant slot-order check was withdrawn after
the construction order and enumerated occurrence issuer were inspected.
Claude subsequently accepted the exact final patch/library reconstruction and
then the corrected evidence packet. Its initial diagnostic-package/README
findings were withdrawn after it inspected the current repository package,
rather than stale scratch copies. This verdict permits retention only, not
live-tool adoption or acceptance of the whole shared compiler worktree.

The immediate next proof-path step is a canonical split-file DAE replay with
structured external-inventory checking, then the actual iterator model/emission
boundary. The existing F64 storage model is supplied deliberately by the pilot;
its absence from a standalone invocation is not evidence of a new Rustc bug.
Do not broaden that model's reviewed domain or external inventory merely to
make generated Lean compile.

The split-file diagnostic8e7951/0 has now run on the same DAE LLBC with checks,
strict joins and emitted JSON. Its manifest requests12 external functions
beyond to_bits and two additional collection types, HashMap and HashSet. This
violates the current pilot's declared external boundary; no models were placed
and no inventory allowance changed. Both census helpers are called by the actual
admission root, so pruning them is not a faithful path to this proof. This
diagnostic is not a successful canonical proof replay.

Remaining general obligations: requested-MIR preservation, inherited
predicate-only lifetime cases, standard-library/auxiliary fact production,
non-Linux executing-image identity, full translator regression review, and live
toolchain integration. Old namespace directories are not automatically deleted;
cleanup needs an explicit safe ownership policy. No blanket Cargo cleanup or
user-artifact deletion was performed in this cut.

No golden admission, new semantic proof, or quick/full/CI green is claimed.
Those integration gates have not run for this unadopted tool cut.
UnitDerivative remains **1/18**.
