# Paired closure replay at campaign wind-down

Status: **experimental evidence, not independently accepted or adopted**.
Frozen for preservation on 2026-09-09 at the user's direction to wind down
the Rust-to-Lean translation campaign. SPEC_0033 sections 2a/4 and the
experimental SPEC_0037 sections 2/6 govern the recorded scope.

## Result

Fresh frozen Charon candidate4 (29d5b284) plus the Box-slice Aeneas candidate
(e91ca476) translates the original preserved-signature and simpler-closures
fixtures. Both generated Types and Funs modules pass pinned Lean4.31.0.
Five universal value-preservation laws pass with no axioms. They concern
the actual generated callers, not a handwritten replacement compiler.
They do not prove pointer identity, borrow-model soundness or the translator.
Original native pointer tests separately pass (one preserved-signature test,
two simpler-closures tests).

The final source mutation adds an assertion to captured_move and unexpectedly
panics at42. Its generated module still has eight functions, zero opaque
functions and passes Lean. The SAME laws reject it (a3a689/1).
MutationWitness proves that fault only on the mutant (7ea73e/0), using exactly
propext, Classical.choice and Quot.sound; the positive module rejects it.
Native tests also reject the mutant; original/mutant scoped strict Clippy,
rustfmt and the source mutation patch application check pass.

An earlier mutation returned a promoted static reference. Aeneas rejected it
with a bottom-value error before proof checking. It is preserved as an
unresolved translator limitation, not counted as a successful proof-fault
control. Intermediate witness attempts used the wrong numeric notation and
axiom-message expectation; their failed commands remain in the receipt.

## Regression diagnosis and limits

Old and fresh two-region LLBC embed identical Rust source. The old reference
closure call_mut lacks capture region0 outliving output region1; the fresh
producer emits that relation in addition to receiver well-formedness.
Fresh source emission succeeds with neither --monomorphize-mut nor --start-from,
with either, and with both. The old LLBC still fails the current consumer.
Thus these flags are not necessary to avoid the historical fixture failure.
This is consistent with the retained declaration-owned constructor, not a
new lifetime guess. No generated Lean or LLBC was edited for a positive proof.

The two-region iterator fixture still requires two opaque iterator operations
and reports missing Iterator/Step model fields. It is NOT Lean-admitted here.
Charon14 with the older Aeneas reader fails at JSON decoding; this is an
unpaired schema, not a closure semantic result. The matching reader remains
separate integration work. No blanket method-constraints acceptance follows.

## Reproduction and retained evidence

The literal commands, exits and captured outputs are in the local evidence
directory dev/method-paired-replay-vO9UVA/run-evidence.json, included in the
wind-down backup. Some decoder-error outputs were tool-truncated and are
explicitly labelled. A transient ENOSPC failure and its successful retry are
also retained. Source fixtures remain under charon/fixtures/closure-lifetimes.

The positive imports are generated from LLBC basenames
preserved_signature_no_specialization_no_root and simpler_closures_4.
Compile Types then Funs into isolated module directories before Laws.lean.
The mutant must use its own generated modules and compiled output directory;
never reuse the positive Funs.olean. source-mutation.patch applies to a
disposable copy of the original Rust fixture, not the production worktree.
The witness is intentionally a negative test against the positive translation.

No live pins, compiler implementation or Modelica semantics changed in this
packet. Broader translator gates, quick/full, independent packet acceptance
and UD07 remain open. UnitDerivative stays at1/18 accepted relations.
