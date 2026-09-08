# DAE library boundary: record shape and source-extracted dispatch

Unadopted diagnostic cut, 2026-09-08, predecessor74083371. Governed by
SPEC_0033 §§2a/4 and SPEC_0037 §§2/6 (DRAFT). No Modelica semantics, Rumoca
source, live tool pin, pilot external allowance or hand-written model changed.
Review status: Claude accepted bounded retention of all five files at
2026-09-08T02:49:47Z, including independent positive proof replay. This is not
live-tool adoption or a whole-root proof verdict.

## First divergence and rejected alternatives

The actual root is
`rumoca_phase_solve::scalar_constant_derivative_refinement::admit_dae_profile`.
Its census helpers call concrete slice/mapped iterators. Baseline Charon and
Aeneas complete, but generated Lean initializes three fields absent from the
selected library's trait records: Iterator map/sum and ExactSizeIterator len.
Both census helpers are genuinely called; dropping them is not an admissible
way to prove the root. Rewriting Rumoca merely to avoid those fields is also
unnecessary for this case.

[Upstream PR1131](https://github.com/AeneasVerif/aeneas/pull/1131) is already
the exact pinned revision f9a8e338188447c77f31246892cb9a7a742e58ef. Its
`-filter-trait-methods` option is off by default. Pinned src/Config.ml and
src/extract/Extract.ml implement the option using the modeled trait's own
builtin method catalog. It changes impl-record field emission only; unmodeled
traits retain every method. It does not supply missing function definitions.

The option-only whole-DAE replay changes exactly the three invalid field
assignments. Both structured manifests hash to
`2cf22e026edd4ccfd83ae180f7456e8dd3ce12b7dc17a4c28b6f0a5f47283e85`, and
the external-function templates are byte-identical. This is a record-shape
correction, not elimination of unmodeled calls or a preservation theorem.

The [Rust fixture](../../../charon/fixtures/dae-library-boundary.rs) separates
concrete mapped receivers from a generic `I: ExactSizeIterator`. The former
Lean-checks with the option; the latter still refuses the absent len projection.
Consequently this is not general iterator support, and the option is not an
admission mechanism. Overrides and generic dispatch need their own semantic
correspondence; the library's partial trait shape must not silently authorize
unmodeled behavior.

## Translate bool::then from Rust, not a replacement model

The DAE manifest initially requests13 external functions, including the
already permitted to_bits, and two additional collection types. The first
unpermitted function is core::bool::{bool}::then. Adding only the exact
Charon selector `--include=core::bool::*::then` extracts its real pinned
standard-library body. No blanket core inclusion or authored replacement is
needed. The body becomes transparent in both the small fixture and actual DAE
output; the DAE external-function count decreases from13 to12. The remaining
boundary is still unadmitted.

The pinned Rust bool.rs input hashes to
`7e96cdc4559d0884c027ffc5de3c6e5bdaf0a0dbdc0e01339159f2062ce05a80`.
The fixture hashes to
`db73b844479638c97e372b8d85b31e3e7292e6a87f150eb14f2ee76d78fbde91`.
The actual DAE replay is source extraction, not a successful whole-root Lean
proof. The fixture's generated module has three transparent functions and no
external function axioms.

[BoolThenLaws.lean](BoolThenLaws.lean) imports that freshly generated module.
It proves disabled-result independence, enabled value/failure/divergence
propagation, and the actual fixture caller's disabled result. Exact axiom
guards admit respectively none, propext/Quot.sound, and the standard three
propext/Classical.choice/Quot.sound. They do not admit external function axioms.
The caller law's Classical.choice comes from constants in its statement
(including the library's Slice.index_usize), not an added proof premise.
These are functional laws about the translated values, not a theorem about
allocation, destructor effects, physical reference identity or effect counts.
Pinned Rust/Charon/Aeneas and the existing Aeneas library semantics remain
trusted. Native fixture tests independently exercise lazy mutation, pointer
identity, empty/multiple-element counts and sums.

The [guard mutation](mutations/invert-caller-guard.patch) applies to a copied
Rust fixture, never to generated LLBC or Lean. The inverted caller still
compiles/extracts/translates, but the unchanged Rust identity/laziness test
panics on a disabled empty slice and the unchanged disabled_reference theorem
fails semantically. The proof's dependency guard also rejects Lean's error
recovery sorryAx; no sorry was added to the proof or generated source.

## Reproduction

Use the candidate pair documented by the preceding
[transport packet](../../../charon/candidates/method-constraints/transport.md).
The measured frozen Charon binary hashes are:

- launcher: ae9bbe39611ebc6217edb14d73933223868ed768372c8db9b7779c9dbde9526c
- driver: 538716917f5c8b2da5face7c100e9b70c7ff6dd389900709fe9fb91733deb00b

This pair predates only the transport checkpoint's RustcRejected error-class
mapping and test-only additions; those error branches are not exercised by
these successful extractions. Aeneas is the method-constraints candidate from
[its recipe](../method-constraints/package.nix), at
`/nix/store/v2br63mjn55pkcfdzf6inyg007l36sj7-ocaml5.2.1-aeneas-0.1.0/bin`.
Rust uses nightly2026-08-18 and its full-MIR sysroot; Lean uses4.31.0 and the
existing lean-pilot library search path. Use four workers and90-second fixture
or180-second DAE watchdogs. The following are diagnostic commands, not a new
shell runner or a replacement for cargo xtask verify quick/full.

From the repository root, with CHARON, AENEAS and OUTPUT explicitly set:

```console
rustc --test --edition=2024 infra/verification/charon/fixtures/dae-library-boundary.rs -o "$OUTPUT/native"
"$OUTPUT/native" --test-threads=4
clippy-driver --test --edition=2024 --emit=metadata infra/verification/charon/fixtures/dae-library-boundary.rs --out-dir "$OUTPUT" -Dwarnings -Dclippy::all -Dclippy::too_many_lines -Dclippy::excessive_nesting
rustfmt --edition=2024 --check infra/verification/charon/fixtures/dae-library-boundary.rs
"$CHARON" rustc --preset=aeneas --error-on-warnings --start-from=dae_library_boundary::conditional_reference --include='core::bool::*::then' --dest-file="$OUTPUT/BoolThenSource.llbc" -- --edition=2024 --crate-name dae_library_boundary --crate-type lib infra/verification/charon/fixtures/dae-library-boundary.rs
"$AENEAS" -backend lean -checks -strict-joins -sequential -emit-json -dest "$OUTPUT" "$OUTPUT/BoolThenSource.llbc"
```

For the concrete/generic controls, use roots mapped_count/generic_count and
dest-file basenames MappedCount/GenericCount, omit the bool selector, and
compare extraction with and without -filter-trait-methods. Do not fill or
install generated external templates as proof models.

In the pinned Lean environment, obtain `RUMOCA_PILOT_LEAN_PATH` from
`lake env printenv LEAN_PATH` in infra/verification/lean-pilot. With OUTPUT
absolute, compile the generated module from its own output directory, then
check the retained proof by its absolute path:

```console
env LEAN_PATH="$OUTPUT:$RUMOCA_PILOT_LEAN_PATH" lean -j4 -M4096 -DwarningAsError=true -o "$OUTPUT/BoolThenSource.olean" "$OUTPUT/BoolThenSource.lean"
env LEAN_PATH="$OUTPUT:$RUMOCA_PILOT_LEAN_PATH" lean -j4 -M4096 -DwarningAsError=true "$REPOSITORY/infra/verification/aeneas/candidates/library-boundary/BoolThenLaws.lean"
```

The DAE replay keeps the original root and includes rumoca_ir_dae,
rumoca_ir_solve and rumoca_core; the sole added selector is core::bool::*::then.
Use charon cargo, `-- -p rumoca-phase-solve --lib`, and Aeneas
`-filter-trait-methods -split-files -emit-json -gen-lib-entry` in addition to
checks/strict-joins/sequential. This does not run the canonical inventory or
proof admission; the manifest still violates that boundary.

## Evidence and limits

Fixture/DAE output: `/tmp/rumoca-dae-library-boundary-tk2Ela`.
Option-only whole-DAE comparison: `/tmp/rumoca-cross-crate-facts-GBMOCE`.
Copied mutation: `/tmp/rumoca-bool-guard-mutation-mIbCWP`.

| Claim | Evidence |
|---|---|
| Record-shape control | VERIFIED: mapped-count Rust/Charon/Aeneasb8b168/0; unfiltered Leanb6a0cf/1, filtered Lean3218e4/0. Four existing external axioms remain; manifest bytes unchanged216bf1e0 |
| Generic boundary | VERIFIED: Charon/Aeneas41c736/0; Lean1061ad/1 refuses the generated generic projection of ExactSizeIterator.len |
| Source-extracted bool fixture | VERIFIED: final source Charon/Aeneasd0a967/0; final generated module and unchanged laws both exit0, recorded in run-evidence.json |
| Native/lint/format | VERIFIED: final fixture f90ff3/0,3 native tests; strict Clippy and rustfmt pass, no lint suppressions |
| Rust mutation | VERIFIED: refreshed final fixture plus contextual patch544ee4/0; nativef7b24c/101, Charon/Aeneas06be2a/0. Final mutated generated module exits0; the unchanged proof exits1 specifically on disabled_reference, recorded in run-evidence.json |
| Actual DAE progress | VERIFIED: Charonce9edf/0 in63s and Aeneas432a89/0 in94.25s. bool::then is transparent; total unmodeled functions13->12. HashMap/HashSet and other unmodeled calls remain |

The final [process receipt](run-evidence.json) retains actual terminal results
and command arguments for both positive and negative proof replays, with equal
before/after input hashes. A successful Lean command is silent: an empty log
alone is not evidence of success. The receipt's observed exit_code, rather
than its expected_exit label, is the recorded outcome. These are local execution
records, not signed attestations or a replacement for replaying the commands.

No whole-root proof, new UnitDerivative relation, translator-preservation
theorem, golden admission or live-tool adoption. Quick/full/CI were not run:
this cut changes only unadopted diagnostic inputs and proofs, not the compiler
or its live verification command. Full upstream regression is not rerun by
these fixture checks. UnitDerivative remains1/18.
