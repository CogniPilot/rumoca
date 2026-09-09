# Specialized parent-trait contracts: finite-demand candidate

Status: **EXPERIMENTAL / REVIEW PENDING**, not an accepted or adopted fix.
Writer: Codex. Reviewer requested: Claude. Governing process: SPEC_0033 §§2a/4/6
and the experimental SPEC_0037 §6. No Rumoca source, live tool pin, checker,
external allowance or proof count change; still 1/18 UnitDerivative relations.

## Current result and limits

The three previously failing source regressions now pass. An added fourth
regression preserves distinct associated types across a noninverse cycle.
The actual specialized DAE extraction also passes Charon's unchanged checker;
the composed Aeneas replay subsequently refuses a specialized trait-implementation
name/generic-argument mismatch. There is no newly admitted Lean relation.
Terminal replay commands and frozen hashes are in `finite-demands-evidence.json`.

The complete upstream UI run is **421 pass / 19 fail / 2 ignored**. All 18
predecessor failures remain, plus `associated_types/issue-1260-self-ref-assoc-const.rs`:
the candidate retains `Field::Packing` instead of lifting it. This is a visible
representation/lifting-capability difference, not a green gate or an accepted
snapshot update. The associated-type information is still explicit, but the
retained-trait selection needs adversarial review before adoption. The first
experimental UI run also exposed 27 sparse-declaration-slot panics; the producer
now enumerates actual declarations and checks missing slots before cycle marking.
Those 27 new failures are absent from the final full UI run.

`finite-demands.patch` changes Charon's associated-type demand owner and, because retention
selects the trait being processed rather than the owner of the demanded type, can change the shape
of any lifted translation, not only partially monomorphized ones: the final full UI run records one
new failure, `associated_types/issue-1260-self-ref-assoc-const.rs`, under plain
`--lift-associated-types`;
`cyclic_demands.rs` is its private helper. `package.nix` composes the frozen
explicit-Self candidate and installs the helper and native fixtures. The recipe
evaluates; the package has not been built with Nix. The measured native build
uses the existing pinned Charon development environment. `aeneas-replay.nix`
composes the frozen method-constraint, drop-control, builtin-identity and
specialized-name candidates without editing them or changing live pins. Its
successful build is not acceptance of those pending-review dependencies.

## Construction proposal, not a preservation proof

Before mutating any declaration, the producer computes a complete demand schedule:

1. Identical associated-type paths require one parameter. Distinct associated
   types and distinct clause witnesses retain distinct demand identities.
2. Recursive demand edges use the previous metadata round. A source path can
   shorten only when its complete endpoint trait predicates normalize equally
   under declared equations, including generic arguments. Trait-ID equality
   alone, shared Output/Residual shapes, or an assumed inverse Try law is insufficient.
3. Quantified constraints are excluded from the new equality justification;
   existing lifetime-erasing approximations are not used to authorize a rewrite.
   The normalizer accounts for binder depth and guards recursive substitutions.
4. An unresolved path causes associated types to remain explicit and the metadata
   schedule to restart. Stable unresolved demands or repeated nonconvergent
   schedules produce an error before any declaration rewrite. No pass is replayed
   over already-mutated IR, and no final type check is disabled.

Termination rationale (INFERRED, not machine-proved): each successful path
contraction deletes a nonempty segment; remaining unknown paths have no repeated
trait ID. The finite source graph therefore bounds these paths. Retention grows
monotonically over a finite trait set, and for a fixed retained set, repeated
complete demand schedules refuse instead of looping. The review must scrutinize
this argument, binder/equality soundness, and retained-trait selection.

Cost is also a review obligation: this adds metadata rounds, path comparisons,
and saved schedules inside Charon. It makes no change to Rumoca's emitted code,
runtime DAGs, arena branding, or production compiler speed, but extraction cost
has not been benchmarked systematically. Do not call it an optimal algorithm
solely because these extractions complete.

## Actual producer mutation control

`predicate-id-only-mutation.patch` replaces complete-predicate equality with
trait-ID-only equality. The original three cases **did not catch it** (f30c87/0).
`cyclic-distinct-associated-source.rs` therefore adds two legal Family implementors
sharing Item/Canonical but differing in Tag (`u8` versus `u16`). Native Rust
compilation and behavior pass. The incorrect producer now supplies a `u8`
associated-type argument where `u16` is required, and the unchanged Charon checker
rejects the fourth regression. The final-source mutation replay5d8e8c/101 confirms
three pass / one fail; restoration reestablishes the canonical helper digest.
This is a concrete mutation witness, not a proof that every bad rewrite is caught.

The current four-case harness is `specialized_traits.rs` (f149b64a).
Reverse `distinct-associated-regression-extension.patch` to reconstruct the
historical three-case89841b04; then reverse `native-regression-extension.patch`
to reconstruct the original single-casebd118dcb. Earlier receipts bind those
earlier bytes, not the current producer or four-case harness.

## Historical diagnosis before the producer candidate

The predecessor is the frozen explicit-only Self fix in
[explicit-substitution](../explicit-substitution/README.md), patch8334ca38,
launcher e8011f70 / driver13ff97d7. It removes the earlier panic but preserves
the final type-checker's rejection on the actual DAE root.

The historical three-case `specialized_traits.rs` checked sources through one shared helper: the
frozen optional-borrow source `../explicit-substitution/trait-self-source.rs`,
`cyclic-assoc-source.rs`, and `cyclic-noninverse-source.rs`. It uses Charon's
existing Rust-source test utility, with the Aeneas preparation flags explicitly listed
but **without unbinding item variables**. This is important: partial mono runs
before that final unbinding pass, so feeding it an already-unbound extraction
would not reproduce its real input contract.

The native test runs the real final type checker on its predecessor: zero
errors. It then calls only the actual partial-monomorphization pass, requires
that specialized trait declarations were created, and invokes the unchanged
checker again: one error. Typed inspection reports
`(implementation, expected parent trait, supplied parent trait) = (3, 2, 5)`.
These IDs are observations from this run, not hard-coded acceptance criteria.
The implementation's declared parent remains the generic Residual trait while
its supplied witness refers to a concrete specialized Residual declaration.
The original single-case fixture's digest remains in `run-evidence.json`.
The historical three-case fixture and its terminal results are bound in
`cyclic-constraints-evidence.json`.

`cyclic-assoc-source.rs` reproduces the same class using two user-defined
traits, TryLike and ResidualLike, without Option, `?`, or std helper bodies.
Rust accepts it; partial mono rejects its parent contract. Thus the failure is
not specific to std extraction or Option spelling. Full rustc-backed
monomorphization accepts this concrete input as a diagnostic reference only;
it is **not adopted as a replacement path**, and no claim is made that it solves
generic compiler-core extraction or the partial-mono defect.

## Cyclic equalities: adversarial design guard

`cyclic-noninverse-source.rs` matches the weaker bound of the real Rust Try
trait: it does **not** require `Residual::TryType = Self`. Two distinct types,
Carrier and Alternative, implement TryLike with the same Output and Residual.
The reverse residual trait chooses Carrier. Native compilation263e35/0,
warning-denied lint9dafd5/0 and the borrow-preservation testc4ecef/0 pass.

The source's generic `revisit_residual` function also type-checks the valid
cycle closure: the declared Output/Residual equalities return to the same
residual predicate. This is Rust type-checking evidence, not a new Lean proof.
Do not replace that justified closure with identity of arbitrary TryLike
implementors. Applying `assume-inverse-probe.patch` to a separate source copy
requires that false stronger law; Rust rejects it881dd5/1 with E0271 because
Carrier and Alternative are different types. No producer was changed by this
source-level design probe.

Before the producer change, all three phase-isolated tests were red14de28/101:
one parent mismatch for Option, one for the explicitly inverse cycle, and two
for the noninverse cycle. Each input passes the unchanged predecessor checker.
That historical native test digest is89841b04; scoped Clippy88dee9/0 and formatting
9b6603/0 pass. Latest UnitDerivative8a7762/0 passes5/5. No candidate acceptance,
compiler capability change, or additional UD relation follows from these tests.

## Alternatives tested and rejected

| Probe | Observed result | Conclusion |
|---|---|---|
| Native isolated partial mono | 1e2711/101, predecessor checker0 → checker1, IDs2/5 | First divergence is the producer, not the final checker |
| Repeat trait and associated-type normalization | 28cbef/101, still IDs2/5 | An extra cleanup pass is insufficient |
| Normalize, then specialize again | bcb3f8/101, now IDs7/5 | Repeating passes does not establish one canonical specialization identity |
| Ordinary user-defined cyclic traits | 9feef0/1 | Same mismatch without standard-library dependencies |
| Full mono concrete reference | 5298fc/0 | Concrete Rust resolution works; not a generic proof or adopted workaround |
| UnitDerivative | b7f811/0, 5/5 | Production compiler spine remains green |

The two `*-probe.patch` files change only the native diagnostic test. They
are not production changes. The test copy was restored afterwards. No
`--no-typecheck`, source exclusion, generated-body edit, or silent omission was
used in any probe. Invalid LLBC was not sent to Aeneas.

## Historical owner analysis

`expand_associated_types::compute_trait_modifications` conservatively retains
associated types when it finds a cycle. Later partial mono classifies
`TyKind::TraitType` as uninfected, so a symbolic parent and its concrete
implementation can select different mutability shapes. This mechanism is
consistent with the measured mismatch; the complete producer repair is not
yet established by that diagnostic cut. The candidate above now repairs the
measured three source cases; broader preservation remains unproved.

The first TryLike/ResidualLike example has explicit inverse equalities; the
second forbids assuming them. These examples constrained the current design:
canonicalize repeated equal residual predicates using declared equations,
without identifying arbitrary Try implementors or discarding associated types.
No trait-name special case or checker ID relaxation is part of the candidate.

The final pretty-printer shows only reachable declaration groups, while the
type checker examines all retained items. That explains why the failing impl
is absent from `--print-llbc`; it is **not** permission to delete its obligation.
Selective partial-mono tracing also exposes an independent formatter panic at
`ast/krate.rs:143` (missing item during pending construction). It remains a
separate diagnostic issue, not a substitute explanation for the type mismatch.

Upstream history checked: [PR915](https://github.com/AeneasVerif/charon/pull/915)
fixed an older marker-filter constraint loss;
[PR1211](https://github.com/AeneasVerif/charon/pull/1211) disabled default partial
mono; [issue824](https://github.com/AeneasVerif/charon/issues/824) states its
mutable-borrow purpose. None supplies a verified fix for this reproducer.

Historical replay details: `run-evidence.json` and `cyclic-constraints-evidence.json`.
Current installation is specified by `package.nix`; fixture copies in the
build tree are not additional maintained sources. No new runner script, live
toolchain adoption, compiler semantic change, or golden admission is introduced.

## Open

- Refusal reachability. Neither demand-error branch ("Could not satisfy", "did not converge") is
  exercised by any run, and the first looks unreachable because a retained trait exports no paths.
  The refusal is reported through `register_error!`, which increments `error_count` at any level;
  the driver exits nonzero only under `--error-on-warnings`. Every run in these receipts passes that
  flag. Without it the driver exits 0 and the export writes `has_errors = true` and labels the LLBC
  partial; what each downstream consumer does with a partial LLBC is not recorded here. Both the
  reachability of the branches and the consumer outcome without the flag are open.
- Idempotence. No run applies the candidate's pass twice or compares outputs across applications;
  the repeat and respecialize probes ran against the original producer.
- Mutation coverage. One mutation of the new logic, caught only by the distinct-associated case; the
  Option, inverse and noninverse cases survive it. The scheduler, the `previous_trait_paths`
  feedback and the aliasing insert are unmutated.
- The Nix recipe was evaluated, not built; the Aeneas composition hash is unverified here.
