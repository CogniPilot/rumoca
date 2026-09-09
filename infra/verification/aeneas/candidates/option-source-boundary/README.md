# Original-source Option and Result boundaries

Review requested, not accepted or adopted. This is a proof-only source-retention
cut under SPEC_0033 sections 2a/4/6 and the experimental SPEC_0037 sections 2/6.
No MLS semantics, production Rumoca representation, branding, live tool pin or
existing library model changes. UnitDerivative remains 1/18, with no new golden
admission. Writer: Codex; independent review requested from Claude.

## Measured gap and earliest owner

The actual three-root DAE/Solve/catalog-checker replay emitted eight opaque
library functions. Four are available as ordinary pinned Rust source bodies:
Option.copied, Option.flatten, Option.clone and Result.ok. Adding their exact
source includes to the existing extraction removes these four opaques without
a translator patch, authored replacement implementation or additional axiom.

The original three includes exposed Option.clone in the Copy supertrait's
dictionary. Its source retention was declared as a mechanically required
consumer before adding the fourth include. Copy does not call Clone. No claim
assumes arbitrary user Clone implementations preserve values or terminate.

Predecessors are the frozen Charon candidate4 and the frozen expect-model
Aeneas stack. The latter still has separate predecessor/adoption review blockers;
these results are diagnostic evidence, not approval of that whole toolchain.
The reservation and scope extension are recorded in dev/comm_channel.md at
22:11:46Z and 22:15:07Z on 2026-09-08.

## Proofs and source mutation

`fixture.rs` exercises generic copied/flatten and their composition. Its final
generated module contains six transparent function entries and zero opaque
function entries. `Laws.lean` proves seven universal laws: source copied,
source flatten, both callers, the composition, and None/Some clone dispatch.
Six laws have no axioms; the Some clone law uses only propext. Exact axiom
guards are part of the maintained proofs.

The existing `builtin-identity/ResultLaws.lean` and
`charon/fixtures/option-library-boundary.rs` are reused unchanged, not copied.
Fresh source extraction and Lean compilation pass both existing guarded laws.
Their generic Result.ok law assumes Aeneas's existing drop abstraction; it does
not prove observable destructor behavior for arbitrary Rust error types. The
production call here consumes the checked integer conversion's TryFromIntError,
which is a Copy type in the pinned Rust source, not a user-defined destructor.

`wrong-disposition.patch` mutates the Rust copy caller to return None. Both
positive and mutant generated modules compile with the same six transparent
entries and no opaques. The unchanged laws reject the Some branch of
copy_caller, not an unknown symbol or failed translation. `MutationWitness.lean`
proves the mutant's wrong outcome, with guarded axiom sets, and fails against
the positive output. Two native tests also reject the mutant.

An earlier diagnostic mutated the composition instead. This removed the
reachable Option Clone member, so it was unsuitable for the unchanged complete
law set. An extra Charon start selector did not restore the emitted member.
These runs are retained in the receipt but are not the final mutation evidence.

## Actual production integration

The same production DAE projection, Solve projection and full joining checker
remain selected. Structured before/after metadata shows 127 emitted function
entries in both runs, eight to four opaque library functions, zero opaque
first-party functions, and the checker loop/body retained. Remaining functions:

- NonZeroU32Inner.clone;
- NonZero.eq;
- boxed-slice conversion from a shared slice;
- Vec.into_boxed_slice.

The complete generated production modules have **not** been Lean-checked.
Storage correspondence, iterator-interface decisions, canonical adoption and
the complete UD07 proof remain open. The unchanged Iterator/Step model-field
warnings are preserved, not reclassified as missing callee bodies. A reduced
fixture's zero-opaque count is not the complete compiler's dependency count.

## Evidence and reproduction

`run-evidence.json` records exact commands, working directories, expected and
observed exits, output, hashes and explicit claim scope. Replay artifacts are
in dev/ud07-option-source-X392sU, outside target. No generated Lean was edited;
all compiled proof outputs are isolated from the live .lake tree. The existing
expect review's isolated rebuilt standard library is reused and hash-bound.
The three named production files are hash-bound, not claimed to be a complete
transitive source fingerprint.

Verified final checks: seven Option laws; two unchanged Result laws; the two
mutant witness laws and their reverse refusal; four native positive tests;
two of four native mutant tests refuse; strict scoped Clippy; Rust formatting;
mutation patch application check; production UnitDerivative five of five.
The mutation failure is expected, not a green capability test. No compiler
runtime cost or output changes are introduced by this proof-only cut.

Not run: canonical cargo xtask verify lean-pilot (unadopted toolchain and open
full-module contracts), cargo xtask verify quick/full (not a dependency-closed
milestone), full MSL sweep and remote CI. Prior broad-gate failures are not
overridden by these focused results. No M1 closure or second relation is claimed.
