# Source-extracted Option and marker operations

Unadopted diagnostic cut, 2026-09-08, predecessor 56f8a67d.
Governing SPEC_0033 §§2a/4 and SPEC_0037 §§2/6 (DRAFT).
Bounded retention: ACCEPT, 2026-09-08. No Rumoca semantics, live tool pins,
trusted library models, external allowances or normative specs changed.

## Actual dependency and first divergence

The root remains
`rumoca_phase_solve::scalar_constant_derivative_refinement::admit_dae_profile`.
After the [bool source extraction](../library-boundary/README.md), its manifest
still requested opaque Option branch/from_residual, Result ok and PhantomData
equality. These are reached dependencies, not hypothetical library coverage.

The [fixture](../../../charon/fixtures/option-library-boundary.rs) exercises
Result/Option composition and generic marker equality. Baseline extraction
(`baseline_extraction` in the receipt) emits four opaque functions. Exact
pinned Rust-body selectors make Option branch/from_residual and marker
equality transparent (`three_body_extraction`).
Their pinned definitions are in core/src/option.rs:2868/2882 and marker.rs:821.
The fixture still has one opaque Result::ok; nothing authorizes that function.

The actual DAE replay with those three selectors succeeds (`dae_three_charon`
and `dae_three_aeneas`, both exit 0). Its external functions decrease from 12
to 9, including the
already permitted f64::to_bits. HashMap/HashSet remain external types.
Manifest SHA256: ac2199d7a16ca11da0aad354ceee4c8a4cc2f975a72275e7d6022c09caea987b.
This is source-translation progress, not whole-root Lean compilation/admission.

## Proof and negative controls

[OptionBoundaryLaws.lean](OptionBoundaryLaws.lean) imports generated code and
proves four functional relations: exact Option branch projection, residual
totality, marker equality independent of T's equality, and the actual Rust
marker caller. All four exact dependency guards report **no axioms**. The
unproved Result::ok axiom in the generated fixture is not a dependency of any
law. No replacement function, added axiom, sorry or native_decide is authored.

Residual totality eliminates the impossible `some Infallible` case. The
generated panic arm is not erased or assumed unreachable: the existing empty
Lean datatype supplies that proof. The type's correspondence to Rust, Rust MIR
production, Charon/Aeneas translation and existing library semantics remain
trusted. These are value laws, not Rust destructor/allocation, physical memory,
branding, or whole-compiler preservation theorems.

Two native tests cover Ok/Err × Some/None with boundary values and marker
equality for types without PartialEq, including unsized types. The
[marker mutation](mutations/invert-marker.patch) negates the actual Rust
caller's equality result. It introduces no unmodeled PartialEq::ne call.
The receipt records native test failure (`mutated_native`, exit 101), successful
extraction (`mutated_three_body_extraction`, exit 0), and semantic failure of
the unchanged caller theorem (`unchanged_laws_on_mutated_rust`, exit 1). Its dependency
guard also refuses Lean's recovery sorryAx. Only a copied fixture was mutated.

The [process receipt](run-evidence.json) records native/lint/format, extraction
and positive/mutated Lean replays, actual terminal exit codes, full commands,
and hashes. Expected exits are labels, not observations. Tool response
`wall_time_seconds` fields are omitted: they do not establish command duration.
Empty stdout alone is never evidence; this is a local replay record, not an
authenticated artifact or admission schema. The named runs replace earlier
unretained terminal-chunk references; the generated inputs to the proof remain
byte-identical after replaying extraction.

## Result::ok remains an open producer defect

Adding `--include='core::result::*::ok'` fails Aeneas in the fixture
(`all_four_aeneas`) and actual DAE (`dae_all_four_aeneas`), both exit 1 at
pinned result.rs:718. Both outputs are
partial and inadmissible. Their generated `sorry` is not used by these proofs.

The interpreter trace (`join_trace`) and
`InterpMatchCtxs.to_symbolic_value_with_borrows` identify the prior branch join
as the first information loss: `Ok(bottom)` joined with `Err(payload)`
becomes a **whole bottom** self before rustc's drop-control discriminant read.
The earlier tag-only-read hypothesis is rejected: allowing bottom payloads
while rejecting a bottom root cannot repair this execution. No read checks
were weakened. A separately owned prepass candidate must preserve the
remaining control/storage behavior; StorageDead is not a no-op. No success
or adoption is claimed for that proposed fix here.

Rust's [discriminant validity discussion](https://github.com/rust-lang/rust/issues/91095)
also cautions against a blanket raw-memory claim. It is context for triage,
not permission to bypass a failed interpreter check.

## Replay and limits

Tool identities/environment are the unchanged
[library-boundary predecessor](../library-boundary/README.md#reproduction).
Use root `option_library_boundary::*`, crate name `option_library_boundary`,
edition2024 and LLBC basename `OptionBoundarySource.llbc`. Add only:

```text
--include='core::option::*::branch'
--include='core::option::*::from_residual'
--include='core::marker::*::eq'
```

Use Charon `--preset=aeneas --error-on-warnings`, then Aeneas Lean with
`-checks -strict-joins -sequential -emit-json`. Compile the generated module
from its output directory before the retained proof; exact Lean invocations
are in the receipt. Baseline omits these selectors; the failing fourth-body
control adds the Result selector above. For actual DAE, keep the predecessor
root/includes/bool selector and add the same three selectors; Aeneas also
uses `-filter-trait-methods -split-files -gen-lib-entry`.

Outputs: `/tmp/rumoca-option-library-boundary-E7TrsA`; positives in three-source
and dae-three, rejected all-four outputs in source and dae-source. Mutation:
`/tmp/rumoca-option-boundary-mutation-KoQI7p`. Generated files were not edited.

VERIFIED: the bounded native, translation, proof and mutation results above.
OPEN: Result extraction, remaining iterator/collection boundary, canonical
integration, full upstream regressions and live adoption. Quick/full/CI were
not run: only unadopted diagnostic inputs/proofs changed, not the compiler or
live verification commands. No new golden admission or whole UnitDerivative
relation; the count remains **1/18**.
