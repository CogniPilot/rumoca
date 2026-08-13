# SPEC_0050 — Proposal to eFMI: record-typed formal parameters in GALEC

**Status: DRAFT PROPOSAL. Not adopted. Not implemented.**
Pending a standards-text review by Codex (requested 2026-08-12) establishing
whether eFMI 1.0.0 Beta-1 already permits this. If Beta-1 permits it, this
document becomes an implementation slice rather than a proposal. If it does
not, this is the submission text, and adoption ahead of standardization would
require a D12 entry in the SPEC_0042 decision register on the D7 precedent.

This proposal may be rejected. Objection 6 below is, in the author's judgement,
strong enough to defeat it, and it has been put to an adversarial reviewer
rather than argued away.

## 1. The problem, measured

Generated Production C for `Vehicles.Rdd2.NavigationEstimator`, the RDD2
multirotor navigation estimator, current integration tip:

| metric | value |
|---|---:|
| protected functions | 48 |
| **total formal parameters across all functions** | **610** |
| functions with >= 10 parameters | 26 |
| `step` formal parameters | 63 |
| `step` prototype length | 65 lines |

The Modelica source is faithful: `Estimation/MultiSensorInvariant/step.mo`
declares 63 inputs and 15 outputs. Their declared types are 48 `Real`, 12
`Boolean`, 2 `Integer`, and exactly 1 record (`Covariance`).

Those 63 inputs are not unrelated scalars. They form roughly six coherent
groups — previous estimator state, IMU measurement, mocap measurement, GPS
measurement, optical-flow measurement, and tuning constants — each of which is
a natural record.

## 2. Current behavior

rumoca flattens a multi-field record input into one formal parameter per field
*before* GALEC, in the Algorithm Code. Minimal reproduction:

```modelica
record Meas
  Real x[3];
  Real y;
  Boolean valid;
end Meas;
function useRec
  input Meas m;
  input Real scale;
  output Real r;
```

emits

```c
static void useRec(
    BState *self,
    const float m_x[3],     /* one record `m`, three fields... */
    float m_y,
    bool m_valid,           /* ...becomes three parameters */
    float scale);
```

The C is a faithful rendering of the Algorithm Code. The flattening is upstream
of the emitter.

## 3. The argument

**Flattening converts a compile-time-detectable error into a runtime one.**

A reviewer verifying a call to `step` must today check 63 argument positions,
protected by nothing but ordering. Two `Real[3]` arguments transposed — say
`mocapPosition` and `gpsPosition` — is well-typed C, well-typed GALEC, and a
silent numerical defect in flight software. With six record arguments the same
transposition is a type error at the call site.

This is not an aesthetic claim about tidiness. It is a claim that the current
rule removes a class of static check, 610 parameter positions' worth in a
single translation unit, from safety-critical generated code.

**The standard's type system already carries aggregates.** `Covariance`
survives as a record in the Modelica and arrives as `float [15][15]`. Arrays
are aggregates with static extents carried through the signature. The question
this proposal raises is where the line sits, not whether one exists.

## 4. Proposed change

Permit a record type as the declared type of a function formal parameter and of
a function local, with the same static-layout discipline arrays already have:

- extents and field types fully determined at translation time;
- no dynamic allocation, no pointer arithmetic exposed in GALEC;
- no recursive record types;
- assignment and field selection only — no new operators.

In our IR this is structurally already expressible: `VariableDeclaration.ty` is
`TypeRef::Primitive(ScalarType) | TypeRef::Compartment(Name)` and `Parameter`
wraps a `VariableDeclaration`. The obstacle is semantic, not structural: every
doc comment and the validator describe compartments as **state** entities
("Compartment entities are state entities: lexical surface only",
`crates/rumoca-ir-galec/src/validate/names.rs:288`), declared in the block's
protected section via `record … end …;` (S-2.2, S-2.7).

A proposal must therefore either (a) extend compartments to be usable as
parameter types, or (b) introduce a distinct value-record type separate from
the state compartment. **(b) is preferred** — see objection 3.

## 5. Certification and safety analysis

- **Aliasing.** By value: no aliasing, at the cost of a copy. By pointer:
  reintroduces the aliasing question that flat scalars and MISRA C:2023
  Rule 8.14's prohibition on `restrict` currently sidestep. A conforming
  emitter can require by-value for inputs, preserving the "callee provably
  cannot write its inputs" property that C `const` on the current flat
  parameters expresses.
- **WCET and stack.** By-value records cost a copy. This is a real cost and it
  is measurable per call site, not hidden. Relevant context: the RDD2 estimator
  stack chain was reduced 26,344 -> 880 bytes by moving intermediates into a
  block context; a naive by-value record parameter regime could give some of
  that back and must be measured, not assumed.
- **Static dispatch.** Unchanged. No function pointers, no dynamic dispatch.
- **Determinism.** Unchanged. Field selection is a static offset.

## 6. Interaction with the Production Code manifest

Believed to be none: LogicalData maps block-level variables, and a protected
function is `static` and invisible outside the translation unit. In the RDD2
estimator, all 48 generated functions are protected and the header declares
exactly three symbols — the eFMI entry points.

**This belief is explicitly flagged as unverified** and is question 3 of the
standards-text review, because relying on it unexamined is what nearly led the
author to a non-conforming change (section 7).

## 7. Alternatives considered

- **Status quo (flatten).** Rejected on the section 3 argument.
- **Emit a C struct parameter while leaving the Algorithm Code flattened.**
  **Rejected as non-conforming, and recorded here as a near-miss.** The eFMU
  ships `AlgorithmCode/` and `ProductionCode/` bound by a checksummed manifest,
  and Production Code must implement the Algorithm Code. Making only the C
  prettier breaks the correspondence the container exists to guarantee.
- **Refactor the model only.** Grouping `step.mo`'s inputs into records
  improves the Modelica source and is worth doing on its own merits, but under
  current flattening the generated C signature is unchanged. This alternative
  is not exclusive with the proposal and should proceed regardless.

## 8. Backward compatibility

Additive. Existing Algorithm Code with flattened parameters remains valid and
unchanged. No existing conforming producer or consumer is invalidated.

## 9. Open objections

Recorded because they were solicited adversarially, not to be dismissed.

1. **Certification surface.** Beta-1's type system is deliberately thin. Every
   added construct is more semantics to certify, more validator rules, more
   traps. A standard for safety-critical codegen may be right to refuse
   aggregates in signatures.
2. **Aliasing.** See section 5; by-pointer would be a genuine regression in a
   property the current design has for free.
3. **Compartments are state entities on purpose.** Reusing them for parameters
   may conflate two things the standard separated deliberately. This is why
   section 4 prefers a distinct value-record type.
4. **LogicalData ambiguity.** Unverified; question 3 of the review.
5. **WCET/stack regression.** See section 5.
6. **This may be a model-design defect, not a standard defect.** A 63-input
   function is arguably badly factored, and the standard should perhaps not be
   amended to make a badly-factored function more comfortable. **The author
   considers this the strongest objection** and has asked the reviewer to press
   it specifically.

## 10. Adoption plan if accepted

1. Extend GALEC AST + validator for record-typed parameters and locals.
2. Stop flattening record parameters in lowering, so Algorithm Code and
   Production Code both carry the record and stay in correspondence.
3. Regroup `step.mo`'s 63 inputs into records — a pure regrouping: same
   expressions, same evaluation order, bit-identical output, verified against
   OpenModelica.
4. If adopted ahead of standardization, add D12 to the SPEC_0042 decision
   register on the D7 precedent, stating plainly that the emitted Algorithm
   Code is a documented deviation from Beta-1.
