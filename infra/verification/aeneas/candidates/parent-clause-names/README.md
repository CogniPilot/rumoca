# Unique parent-clause field names

Review pending; no live tool adoption or new UnitDerivative relation.
This candidate implements the naming fix for
[Aeneas issue1051](https://github.com/AeneasVerif/aeneas/issues/1051).
Anchors: SPEC_0033 sections 2a/4/6 and experimental SPEC_0037 sections 2/6.
No Modelica semantics, Rumoca representation, branding, Rust source or runtime
cost changes. The existing unadopted expect-model stack is the predecessor.

## Reproduction and owner

Different associated types can require the same parent trait. The predecessor
maps both clause identities to one display name. Field-name reuse is permitted
across records, but this also permitted a collision inside the same record.
Charon keeps the constraints distinct; this is Aeneas extraction ownership.

fixture.rs has two Value constraints with different associated types, plus
methods explicitly renamed ValueInst and ValueInst1. Native tests pass; the
predecessor emits nine transparent functions but Lean rejects the duplicate
record field. The independent Single trait is a noncollision control.

## Producer change

unique-parent-fields.patch registers the other trait members first, then
allocates generated parent-clause names in that trait's field namespace using
the existing fresh-name helper. It reserves the actual registered member names,
not guessed Rust spellings. In the fixture the parent fields become
ValueInst2/3; Single retains ValueInst. Declarations, instances, projections and
parentClauses metadata read the same existing clause-ID-to-name table.

Builtin contract names are not freshened. A colliding builtin contract is
explicitly refused; this branch has mechanism review, not an injected-builtin
negative execution in this packet. Cross-backend behavior is not claimed:
the executed compiler-output checks use Lean's default naming mode.

The actual selected production replay changes exactly three lines: the second
Copy field's name, its parentClauses string, and its instance assignment. All
127 function metadata entries are identical. Four opaque library operations and
zero opaque first-party functions remain; naming success does not discharge
their contracts or the complete production Lean check.

## Proof and mutation

Laws.lean proves generic dispatch to both distinct parent dictionaries and
both methods in the original evaluation order; arbitrary failure/divergence
from those dictionaries is not assumed away. It also proves concrete results
for every U32 and Bool input, plus generic and concrete Single behavior.

Three laws use no axioms. The concrete numeric law uses exactly the standard
Lean foundations propext, Classical.choice and Quot.sound, checked by guards;
it has no new semantic, native-decide or sorry axiom. The initial stronger
zero-axiom expectation for that law failed and is retained in the evidence.

wrong-dispatch.patch swaps the two Rust result-producing calls. Both positive
and mutant generated modules have nine transparent entries and zero opaques,
and pass Lean compilation. The unchanged dispatch and concrete-result proofs
reject the mutant. MutationWitness.lean proves its reversed result for all
inputs and fails against the original module. Native tests also detect it.
No generated Lean is edited to make either outcome.

The operation-only NonZero reducer now passes Lean's syntax/type check with
distinct Copy fields and matching declaration metadata. It still declares
opaque storage types and get; that module's axioms are NOT admitted as semantic
contracts. Its equality is not proved. Source-retaining the actual range type
still fails on TPattern, confirming this naming cut does not erase validity.

## Evidence, limits and reproduction

run-evidence.json records exact commands, terminal exits, hashes, positive and
negative controls and retained failed build/format/guard attempts. Isolated
artifacts live in dev/parent-clause-names-yaqQJe. The recipe adds only the
reviewed source patch to the named predecessor, and embeds its full digest in
the tool version. No live .lake files or generated modules are edited.

The focused checks include native tests, strict scoped Clippy, Rust and OCaml
formatting, translator and mutation patch applicability, fresh Lean compilation,
guarded proofs, source-mutation refusal and production metadata/source deltas.
The current packet is not acceptance of its predecessor chain.

Not run: full upstream Aeneas suite, non-Lean backends, injected-invalid-builtin
metadata, complete production Lean admission, canonical cargo xtask verify
lean-pilot/quick/full, MSL sweep or remote CI. Canonical adoption is not
dependency-closed. UnitDerivative production tests remain the working tracer,
not evidence of another proof obligation or broad gate completion.
