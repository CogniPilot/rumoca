# Explicit-only substitution preserves implicit Self

Status: unadopted diagnostic candidate; closure revision **awaiting Codex adversarial review**.
No live Charon/Aeneas pin, Rumoca IR, checker allowance, or proof count changes.
Still **1/18 UnitDerivative relations**, not another completed relation.

## Contract and first divergence

SPEC_0033 §§2a/4/6 and the experimental SPEC_0037 §6 govern this tool cut.
There is no MLS or target-semantics change. Charon's existing
`TyVisitable::substitute_explicits` contract substitutes only types, regions,
and constants. It must preserve implicit trait witnesses, including `SelfId`.
Full substitution must still require and substitute its supplied Self witness.

Actual failing input: production
`rumoca_phase_solve::scalar_constant_derivative_refinement::admit_dae_profile`,
with source-extracted library helpers and `--monomorphize-mut`. The predecessor
panics in `SubstVisitor::visit_self_clause` while `compute_shape` substitutes
explicit arguments inside an associated-type constraint. The exception is
before Aeneas, not a Lean problem or permission to alter compiler branding.

`trait-self-source.rs` reduces the same failure to a five-line function: `?`
on `Option<&mut u32>`, then increment and return the borrow. Native execution
checks both Some and None. The predecessor panics at the same substitution
owner. A prior ordinary custom-supertrait probe did not reproduce the panic;
it was replaced by this actual optional-borrow reproducer, not counted as a kill.

Competing hypotheses rejected:

- Missing explicit arguments: the smallest failing input is `SelfId` plus
  empty arguments, containing no explicit variable at all.
- Missing implicit witness required by this operation: its documented domain
  and adjacent `visit_clause_var` explicitly preserve implicit witnesses.
- Rust source invalidity: the reduced source compiles and executes natively.
- Missing std facts: the actual DAE replay reaches final type checking after
  the fix, using the existing collector rather than bypassing it.

## Producer delta and controls

`explicit-self.patch` adds the existing explicit-only mode guard to
`visit_self_clause`, plus one API-doc line. No type shape or public item changes.
The remaining full-substitution body, binder visitor, variable substitution,
and metadata/missing-argument refusals are unchanged. Four added source lines;
constant-time visitor branch; no Rumoca runtime or representation cost.

`explicit_substitution.rs` is a native upstream integration test. Its seven
cases cover bare Self, nested parent/associated-type witnesses with all three
explicit argument kinds, a genuine inner lifetime binder, unchanged ordinary
clauses, supplied Self shifted under a binder, and both missing-witness and
missing-explicit-argument refusals. The synthetic AST cases test the utility's
structure, not independent whole-AST well-formedness. The real Rust reproducer
binds the fault to production translation.

Predecessor: 4 pass / 3 panic. Candidate and final restoration: 7/7 pass.
`ignore-full-self.patch` deliberately also skips a supplied full-substitution
witness. The unchanged test fails with retained SelfId instead of the required
bound clause. This mutant is **not** part of `package.nix` and was restored
before final checks. Frozen replay binaries never contained the mutation.

## Measured outcome and remaining refusal

| Claim | Evidence | Scope |
|---|---|---|
| Explicit-only bug reproduced and repaired | VERIFIED native 4/3 → 7/0; over-broad mutation 6/1; restored 7/0 | Utility tests, not a translator correctness theorem |
| Existing library tests | VERIFIED 24/24 | Charon library tests |
| Strict standard lint and formatting | VERIFIED all-target `-D warnings`, Rust formatting | No new lint suppression |
| Optional-borrow native behavior | VERIFIED 1/1, strict fixture lint | Concrete Rust oracle |
| Actual DAE gets past SelfId panic | VERIFIED frozen-binary replay | Still rejected: two specialized Option::Try trait-clause mismatches |
| Reduced source reaches the same next error | VERIFIED one corresponding trait-clause mismatch | Not successful specialized translation |
| Full Charon UI | VERIFIED predecessor 422/19/2, candidate 423/18/2 at the recipe position | The 18 shared failures are identical by name; the one extra predecessor failure is the new fixture, which panics there. |
| UnitDerivative spine | VERIFIED 5/5 after the cut | Production compile, simulation and typed refusals; not golden admission |

Removing monomorphization from the reduced source lets Charon and Aeneas emit,
but the generated Lean fails: the caller expects a backward-function product
from `Option::branch` while the unspecialized body returns only ControlFlow.
This is an explicit negative control, **not** a fallback translation path.
No invalid LLBC or Lean is admitted, and no generated body is hand-edited.

The broader experimental complexity-lint invocation stopped in unchanged
`adt-into::adt_into` (112 lines) and `rustc_trait_elaboration::resolve`
(166 lines). Standard warning-denied all-target Clippy passes separately;
the new source fixture also passes the extra complexity lints.

## Replay, retention and review

`run-evidence.json` binds commands, exit statuses, tool/source hashes, open
failures, and the local diagnostic directory. `package.nix` composes the
retained method-constraints transport recipe and installs the native test.
It evaluates to a derivation but is not a live pin or a Nix build result. Native
replays use the existing upstream source tree with the retained patch; compare
the recorded source hash after reconstructing the patch.

Not run: Nix candidate build; complete upstream root `make test && make clippy`
(the standalone Rust source tree has no root Makefile); whole DAE Lean after
specialization (Charon refuses first); Rumoca quick/full/CI for this unadopted
tools-only cut. The explicit Rust library/UI/all-target lint runs above are
not substituted for those broader claims. A broad-gate failure is never a pass.

Upstream-first check: upstream main at
`52b461e20ccb86200ba68025dd28d5c7a10c8a53` has the same unguarded Self visitor.
[Issue 789](https://github.com/AeneasVerif/charon/issues/789) explains the
special Self clause and binder discipline. Historical
[PR 707](https://github.com/AeneasVerif/charon/pull/707) changes other
monomorphization panic handling, not this explicit-only guard. No upstream
patch was adopted; this four-line fix remains a candidate for upstream review.

Requested verdict: bounded retention ACCEPT/FIX for this invariant and these
bytes, not whole-tool adoption. Next dependency: preserve associated-type
normalization/trait-clause agreement under mutable-borrow specialization on
the same reduced source, then replay the actual DAE without relaxing its
final type checker.

## Same-source pair and registered reproducer

The shared source tree behind the earlier `source_predecessor` and
`source_candidate` records later advanced past this recipe: the finite-demands
and demand-retention patches now apply there. A pair rebuilt from that tree
exits 0 on both sides of `explicit-self.patch` (the substitution panic is no
longer reached and the trait-clause mismatch is resolved by those later
patches), while the frozen candidate launcher still refuses on the same input.
Those two rebuilt runs are recorded as `discovery_failure` records.

The closure records therefore use a clean pair at this recipe's own position:
pinned upstream source plus the box, capture-arguments, constructor and
transport patches, with the candidate adding only `explicit-self.patch`. On
`trait-self-source.rs` with the recorded flags the predecessor panics at
`substitute.rs:117` (exit 101) and the candidate exits 1 at the specialized
`Option::Try` trait-clause mismatch. Both launchers are frozen under
`~/rumoca-checkpoints/2026-09-09-explicit-substitution-8Q30UU`.

`optional-borrow-try.rs` registers the same reproducer in the Charon UI suite
as a `known-failure` case with `no-default-options` and the recorded flags;
`optional-borrow-try.out` pins the refusal text. `package.nix` installs both
under `tests/ui/monomorphization/`, so a build of this recipe runs it. On the
predecessor the case fails (panic instead of the expected refusal); on the
candidate it passes. Full UI runs on both trees share the same 18 pre-existing
failure identities. When a later recipe resolves the trait-clause mismatch,
this case must be re-recorded as a passing translation rather than left as a
stale known failure.

## Open

- `package.nix` evaluates but has not been built with Nix.
- Clippy and rustfmt were not rerun on the recipe-position trees; their sources
  are the frozen patch set plus the fixture, which passes rustfmt.
- Unit-test output for the mutation runs is not retained; the receipt
  paraphrases the failure.
