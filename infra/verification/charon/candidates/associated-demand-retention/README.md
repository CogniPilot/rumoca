# Keep the type-changing cyclic edge explicit

Status: **EXPERIMENTAL / REVIEW PENDING**, not adopted. Writer Codex;
reviewer Claude. SPEC_0033 §§2a/4/6 and experimental SPEC_0037 §6 govern this
verification-tool cut. No MLS, Rumoca IR, checker, live tool pin or proof-count
change. Still1/18 UnitDerivative relations, not golden admission.

## Result

The candidate removes the finite-demand predecessor's new Ring/Field lifting
regression without reintroducing its earlier specialization failures. The same
retention shape now works with reversed declarations; the original pre-finite
producer already failed that reversal. Three retention tests and the unchanged
four specialization tests pass. Both actual producer mutations are rejected,
and restored tests, library27/27, strict all-target Clippy and formatting pass.

Full native UI is422 pass/18 fail/2 ignored. Comparing failure identities with
the finite predecessor's19 removes only issue-1260-self-ref-assoc-const.rs;
there is no new failure and no snapshot update. This is not an all-green suite.
The actual specialized DAE also passes Charon with has_errors=false and298
retained functions. Aeneas still refuses specialized trait-implementation name
generic instantiation; there is no new Lean module admission.

Current code is `growth-edge-retention.patch`, over the frozen
[finite-demand candidate](../specialized-trait-normalization/README.md).
The patch changes only its demand constructor and private cyclic-path helper.
`run-evidence.json` binds source/tool hashes, commands, exits and limitations.

## First divergence and rejected proposal

The unchanged upstream issue-1260 source has Ring::Sub bounded by Field, while
Field extends Ring and declares Packing. The finite predecessor keeps both
traits' associated types. Its missing synthesized Packing witness fails the
new typed test (e5bff8/101) and changes the upstream snapshot.

The diagnostic trace4f141b records both paths: Field→Ring→Field/Packing and
Ring→Field→Ring/Sub. Consumer and final associated-type owner coincide in each;
merely changing to the final path ID would not help. `retention-trace-probe.patch`
reconstructs this instrumentation on the finite predecessor; it is not installed.
An initial launcher-only run did not rebuild the driver and is not trace evidence.
The recorded probe follows an explicit all-binary build081809.

`retention-entry.patch` is a **rejected diagnostic proposal**, not part of
the package. Choosing an active DFS entry restored the original snapshot and
UI counts, but declaration reversal caused a missing Ring::Sub value during
reference update (b266cf/101). The frozen finite predecessor passes that reversal
(189412/0), while the original pre-finite producer fails it (c11357/1).
Therefore matching the existing snapshot was insufficient preservation evidence.

## Producer change

The private normalizer now returns either a normalized path or its unresolved
remainder after justified contractions. It no longer loses that remainder on
failure. Successful contractions still require complete source-constrained
predicate equality; the lifetime/binder restrictions and recursive-substitution
guard are unchanged.

Retention follows the unresolved cycle's source edges and keeps the declaration
where normalized Self changes explicit. Ring::Sub is such an edge; the
same-Self Field→Ring supertrait edge is not. This gives both declaration orders
the same tested shape without trait-name branches or DFS-state heuristics.
When Self is constant but other generic arguments vary, retention keeps a cycle
entry's associated form; this is not an equality assertion. If no owner can be
resolved, the existing conservative current-trait retention remains. The demand
preflight still refuses stable unsatisfied or nonconvergent schedules before
declarations are rewritten. Retention keeps an explicit associated form instead
of treating an unresolved cycle as an equality; whole preservation remains unproved.

`parent_ref` is the shared source-clause/constraint operation for normalization
and retention. The rejected active-entry helper is deleted from the candidate
producer; neither it nor the temporary trace is installed by `package.nix`.
The recipe applies the overlay after the predecessor installs its private helper.
It evaluates, but its Nix package has not been built; native gates use the pinned
development toolchain. There is no new shell runner or live toolchain adoption.

## Adversarial controls and limits

- `cyclic_retention.rs` checks the original and reversed source against one
  shared typed shape assertion, plus Link/Back with a separate hard cycle that
  must not cause the unrelated invertible traits to retain associated types.
- `entry-owner-mutation.patch` ignores the type-changing edge and chooses the
  first repeated trait. All three retention tests fail3eb77b/101.
- `predicate-id-only-mutation.patch` weakens complete predicate equality to
  trait-ID equality. The unchanged u8/u16 regression fails79972a/101. The patch
  is rebased for this helper; the predecessor's context no longer applies.
- Restoration4b0199 passes all7. The frozen DAE launcher/driver were copied
  before either mutation and never contained one.

These are executable counterexamples and regression checks, not a preservation
or termination theorem for Charon. Complex quantified paths, generic-argument-
only cycles and full preflight-refusal coverage still require review and further
controls. The retention policy is not claimed globally minimal or optimal.
The added cycle traversal, predicate normalization and reference clones cost
extraction work; no systematic performance benchmark was run. Rumoca production
runtime, tensor representation and emitted-code performance are unaffected by
this unadopted tool-only candidate.

Independent review of this and its predecessor packets remains required before
adoption/commit. Whole DAE Lean admission, the remaining Aeneas name owner,
quick/full gates and CI are still open. Periodic UnitDerivativea6e71d passes5/5.

## Observed retention rule and rotation evidence

The order test above permutes a cycle with exactly one Self-changing edge, so it cannot separate a
structural owner choice from a coincidental one. The diagnostic packet
`charon/candidates/retention-rotation` runs this exact candidate on cycles with two Self-changing
edges (a two-trait cycle, in both declaration orders and both roots, with and without
`--start-from`; a three-trait cycle with a supertrait edge between the two, in four orders; and a
cycle resolvable by a declared equality, in both orders), all with concrete impls, comparing a
sorted extract of trait headers, retained associated types, implied clauses and impl obligations.
Every cell of every shape produced one extract. The observed rule on those shapes: every trait whose
own demand is cyclic through a Self-changing edge and is not resolved by a declared equality is
retained, so on a two-edge cycle two boundaries are retained, and nothing is retained when an
equality resolves the cycle. Retention is not exclusive, because a retained trait exports no paths,
which is why the retained set is the maximal one and therefore independent of order and root on
those shapes. That is twelve cells over three shapes, not a theorem, and it does not establish
minimality; the policy is now known to retain every cyclic owner rather than one.

## Open

- Cycles with three or more Self-changing edges, generic-argument-only cycles, quantified paths, and
  cycles mixing equality-resolved and unresolved edges are unexercised.
- Only one mutation touches retention logic; the constant-Self fallback, the consumer-id fallback
  and remainder preservation are unmutated.
- No fixture has an impl of a retained trait, so the impl-side consumer of retained demands (new
  associated types the impl must supply) is unexercised; whether any retained trait occurs in the
  DAE root is unknown, and the "298 retained functions" figure is not derivable from this packet.
- Minimality of the retained set is not claimed and not established.
