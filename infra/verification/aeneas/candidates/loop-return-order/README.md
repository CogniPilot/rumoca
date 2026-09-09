# Normalize branch continuations before early loop returns

R3 update 16:18Z: built successfully (8e1285/0, binary a6d9db0a); Claude
gave bounded logic ACCEPT at16:09:35Z. `run-evidence-r3.json` retains the new
23 terminal observations and 49 source/artifact bindings; the older receipt
remains the R1/R2 history. Seven guarded-while laws and six post-loop laws pass
unchanged with axiom guards. Five additional reviewer controls Lean-check;
the nested-loop-return control still refuses. The preserved real Rust payload
mutant emits and Lean-checks, then fails the unchanged semantic laws (0c89f6/1).
Actual catalog checker emits ece4dd/0; its six opaque library functions remain
unadmitted. The tail-call fixture has one shared finish call, versus two in R2
and one in the predecessor. Abort acceptance widening is retained explicitly.
This evidence supplement awaits independent packet closure, not code adoption.

R3 declaration 15:46Z: preserve shared suffixes while rewriting branch returns.
The private recursive helper classifies fallthrough and leaves a tail once
outside its Switch, discarding it only when no branch can reach it. Top-level
fallthrough refuses. Keystone: update_loops/decompose_after and pass order in
PrePasses.ml; no new public type, schema, model or forced consumer. Same writer,
reviewer and reservation. package.nix now selects shared-continuations.patch.
R2's bounded ACCEPT does not transfer to this changed body without review.

R2 review 15:40: bounded logic ACCEPT, packet FIX. Its suffix distribution
copies calls, assertions and non-inlinable assignments, unlike the restricted
join pass. Repeated switches can multiply copies by the product of branch
counts, on top of the existing per-break duplication. The abort-branch control
also widens acceptance (predecessor rejects; R2 translates). These are now
explicit, not hidden by the earlier R1 description below. R3 attempts to remove
that avoidable multiplier; all six reviewer controls must be retained.

Status 15:17Z: ordering-only patch ead72be0 is REJECTED (Claude FIX).
It makes previously accepted post-loop branching fail; evidence:
dev/loop-return-order-review-W6T76i/. The positive subset laws are not general
preservation. Live tools were never changed.

R2 producer declaration, same reservation and owner: extend update_loops's
decompose_after to recurse over structured Switch branches with the remaining
suffix as their shared continuation. Return becomes Break 0, Abort remains
terminal, other statements keep their order; each chosen branch executes its
continuation once. Keep the existing strict checks for nested loop returns and
outer-loop breaks. No new type, consumer, model or Rumoca production change.
R2 was built from loop-continuations.patch; ead72be0 remains only failed
experiment evidence. Required controls now include both review counterexamples
and an early-return payload mutation on the original guarded-while source.

Producer reservation: 2026-09-08T15:01:00Z through 2026-09-08T16:30:00Z.
Writer: Codex. Reviewer: Claude. SPEC_0033 §§2/2a/4; experimental SPEC_0037 §6.
Predecessor: implicit-alias candidate, binary 10916246. Keystone file:
PrePasses.ml, update_loops/decompose_after and apply_passes function_passes order.
No type-shape, schema,
forced consumer, library model or Rumoca production change. Candidate only;
live pins stay untouched pending independent review.

Invariant: update_loops must see supported shared return continuations in their
own branch before deciding a guarded loop cannot be normalized. Existing
remove_useless_joins already moves its permitted suffixes into mutually exclusive
branches and truncates them after terminal statements. It currently runs too
late, after update_loops has discarded the body for lack of a local return.
R1 moved that existing pass once before update_loops. R2/R3 additionally
rewrite terminal returns in branch-shaped continuations within update_loops;
the original restricted join helper itself is not copied or changed.

Actual production failure: UD07 check_projected_variable_catalog_refinement.
Reducer: dev/ud07-loop-return-Wkt24U/loop-return.rs. Both guarded iterator and
guarded while forms refuse in update_loops; top-level forms proceed. Native
success/error controls and strict Clippy/format pass. This rules out iterator
support as the cause of this specific prepass failure, not as a later dependency.

Related upstream issue: https://github.com/AeneasVerif/aeneas/issues/822,
including https://github.com/AeneasVerif/aeneas/issues/822#issuecomment-5292628640.
The latter has an Option-? guard and two loop returns, like a shared structured
continuation. Search also found #1221 (nested-loop opacity), not a repair, and
merged #600/#918, not a new applicable patch. No upstream code has been adopted.

Required evidence: fresh guarded and flat controls, actual UD07 body replay,
no opaque failed first-party body, generated behavior proofs and independent
review. Extraction alone is not acceptance. Abort-branch acceptance widens as
noted above; nested-loop limits remain. Tests and
receipts will name any remaining refusal. No compiler-relation credit yet.
