# NonZero original-source copy proof and diagnostic

Status: copy proof ready for independent review, not accepted or adopted.
Governing anchors: SPEC_0033 sections 2a/4/6 and experimental SPEC_0037 sections
2/6. No Modelica semantics or Rumoca source, representation, branding, live pin,
or existing storage model changes. UnitDerivative remains 1/18.

## Copy continuation

The unchanged fixture is now extracted from clone_optional only, retaining the
original inner Clone, generic outer Clone, Option Clone and generic caller.
The five emitted functions are transparent. Both positive and source-mutant
modules compile against the unchanged pilot models/TypesExternal.lean, compiled
directly into an isolated output directory. No generated external-template
axiom is used, no handwritten operation model is introduced, and no generated
Lean is edited. The frozen parent-clause-names candidate fixes the previously
discovered field collision; it and its predecessor stack still await adoption.

CopyLaws.lean proves four universal identity/dispatch laws with no axioms.
The existing storage correspondence remains an explicit trusted boundary;
quantification over its broadened domain is not proof of Rust constructibility.
The original copy bodies never read the model's integer payload or assume that
its range predicate holds. This does not admit numeric get or equality.

copy-wrong-disposition.patch changes the Rust caller to perform the clone but
discard its result and return None. Keeping the call preserves its dependencies.
Both versions emit the same five transparent entries with zero opaque functions
and Lean-check; the unchanged Some-case law then rejects the mutant.
CopyMutationWitness.lean proves the mutant discards every input, without axioms,
and fails against the positive output. Native tests also detect the lost Some
payload. The private standard-library Clone body itself was not source-mutated;
that is not claimed by this caller-mutation control.

Adding only the inner-clone source include to the actual three-root production
extraction retains all127 function names and reduces opaque libraries4 to3,
with no opaque first-party function. Type declarations are byte-identical.
The newly emitted clone declaration exactly matches the proved reducer's
declaration; its whole production module has NOT been Lean-checked. Remaining
operations are NonZero.eq and the two boxed-slice conversions. Storage/map and
iterator-interface obligations remain open, as do canonical adoption and UD07.

copy-source-evidence.json records exact commands, results, source/model/tool
hashes, reducer/production counts, body comparison and proof limits. Artifacts
are in dev/nonzero-copy-IuyLJJ. Two native tests, strict Clippy, formatting and
patch checking pass. Production UnitDerivative63fb7d/0 passes5/5. All evidence
is experimental and does not close M1 or increment the eighteen-relation count.

## Initial diagnostic: reproduction and earliest owners

The fixture uses actual NonZeroU32 equality and generic Clone dispatch.
Its two native tests, strict Clippy and formatting pass. Original-source Charon
extraction succeeds and retains Rust's range type U32 in 1..=4294967295.
Aeneas fails on that TPattern, then reports a secondary missing-type exception;
it emits no Lean. This is not evidence of broken Charon range extraction.

[Charon1212](https://github.com/AeneasVerif/charon/pull/1212) already implements
pattern-type extraction. Its [Aeneas companion1083](https://github.com/AeneasVerif/aeneas/pull/1083)
updates the pin and unsupported-type fallbacks, not the missing semantics.
Aeneas838 concerns a different string-pattern trait and is not a relevant fix.
No assertion that all upstream alternatives have been exhausted is made.

A second source-retention probe keeps the existing opaque storage boundary
and does not source-retain get. It emits the original inner Clone body as
identity without inspecting a payload. Equality calls opaque get and remains
unproved. Generated type/function axioms in that diagnostic are NOT admitted
contracts; no complete module or theorem from it has passed Lean.

Both that diagnostic and the previous actual production Types.lean contain
duplicate markerCopyInst fields for Copy(Self) and Copy(Self_NonZeroInner).
[Upstream1051](https://github.com/AeneasVerif/aeneas/issues/1051) independently
reports the same naming defect. The next bounded implementation cut is
parent-clause-names, not hand-editing generated fields or removing a constraint.

## Initial diagnostic scope and outstanding work

Copying a value unchanged need not inspect its nonzero payload. At the initial
checkpoint, establishing that identity awaited naming repair and proof checks;
the continuation above now passes those checks, with independent review still
pending. Numeric get/equality additionally
require a reviewed representation correspondence or proper source translation.
The Rust range guarantee will not be erased to manufacture translation success.

At that initial checkpoint the actual selected production measurement was127
function entries, four opaque library operations and zero opaque first-party
functions; the copy continuation above now measures three. The tiny
operation-only fixture has a different dependency set; its counts do not replace
that measurement. No M1 milestone closure or UD07 credit is claimed.

run-evidence.json binds commands, observed exits, source/artifact hashes and
unrun checks. Artifacts are in dev/nonzero-source-Nweqqt, outside target.
Fresh unchanged production UnitDerivative742551/0 passes5/5. Canonical proof
replay, quick/full, MSL and remote CI are not run for this diagnostic because
the necessary source/contracts and tool adoption are not dependency-closed.
