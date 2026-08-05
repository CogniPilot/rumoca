# SPEC_0037: Formally Verified Compiler

## Status
DRAFT

> **Rumoca and its phases are not formally verified.** This draft records work
> in progress; promotion still requires reviewed, machine-checked evidence.

## Summary

Rumoca should support a machine-checked, compositional refinement proof from a
defined Modelica profile through its IR phases to observable target behavior.

## Specification

### Verification Claim

The intended top-level mathematical claim is:

```text
compile(source) = Ok(target)
  implies behaviors(target) subseteq behaviors_modelica(source)
```

The independent language-completeness theorem is:

```text
frontend_accepts(mls_version, source) iff well_formed_mls(mls_version, source)
```

For a deterministic certified experiment, the required executable corollary is
stronger:

```text
compile(profile, source, experiment) = Ok(target) and assumptions(profile)
  implies simulate(target, experiment) = Ok(target_trace)
      and distance(target_trace, modelica_trace(source, experiment)) <= tolerance(profile)
```

Certified compilation therefore proves total execution and bounded trace
refinement; a normal wrong trace is a counterexample.

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| The supported Modelica profile is closed and versioned | formal semantics | Coverage must be precise |
| Every IR has a well-formedness predicate | proof development | Construction claims need meaning |
| Every IR has observable behavior semantics | proof development | Refinement compares behavior |
| Every successful phase proves target well-formedness | phase theorem | Invalid output is impossible |
| Every successful phase proves behavioral refinement | phase theorem | Compilation cannot invent behavior |
| Phase theorems compose into one compiler theorem | proof development | End-to-end claim follows locally |
| Unsupported inputs return typed errors | production phases | Unverified fallback is prohibited |
| Proven features and assumptions are machine-readable | verification manifest | Claims remain auditable |
| Certified compilation implies accurate simulation | compiler theorem | Success is an end-to-end claim |
| Normal-but-inaccurate execution is a proof counterexample | verification process | Completion cannot mask wrong semantics |
| Frontend acceptance is equivalent to pinned-MLS compliance | language theorem | Soundness and completeness are required |
| Formalization gaps are informational, not compile failures | assurance manifest | Proof progress cannot narrow behavior |
| MLS ambiguities retain submission-ready issue records | verification gap tracker | Upstream clarification stays actionable |

Behavior includes continuous trajectories, superdense events, clock ticks,
discrete updates, assertions, termination, external observations, and permitted
resource failures. Equality may replace subset refinement only where source and
target semantics are deterministic and total.

Pointwise reference comparison checks refinement only for an identifiable
trajectory. A typed `trace_nonidentifiable` profile marks the method inapplicable,
not certified. Stochastic evidence names typed Solve-IR random operations;
deterministic-chaotic evidence supplies a positive finite Lyapunov lower bound,
sample count, and artifact digest. Model identity, OMC output, and failed bands
cannot derive either classification.

Profiles list replacement obligations: generator/seed parity and statistical
refinement for stochastic traces; invariant and statistical refinement for
chaotic traces. Until machine-checked discharge, they remain visible,
non-strict-high, and outside the pointwise denominator. Invalid evidence fails
closed.

A reviewed model-specific exclusion may identify a limitation of the OMC
pointwise test, never an exception to compiler refinement. It stays visible and
non-strict-high but is not a counterexample; unreviewed misses are. SPEC_0033
§6a owns reasons and accounting.

### Phase Proof Obligations

| Phase | Required theorem or witness | Observable relation |
|---|---|---|
| Parse | Successful AST derives from the source grammar | Tokens and spans correspond |
| Resolve | Every use selects the unique MLS-visible declaration | References preserve denotation |
| Typecheck | Every operation is defined for its inferred type | Elaboration preserves meaning |
| Instantiate | Inheritance, redeclare, and modifications are applied | Instance behavior equals class behavior |
| Flatten | Live instances map bijectively to flat identities | Equal modulo structural renaming |
| ToDAE | Appendix-B lowering rules preserve source solutions | Equal after hiding auxiliaries |
| Structural | Rewrite certificate preserves the solution set | Equal under reconstruction map |
| Solve | DAE coordinates map to defined executable slots | Exact evaluation commutes |
| Codegen | Target operations implement Solve operations | Target behavior refines Solve |
| Wire | Decode reconstructs only checked current IR | Round trip preserves identity |

Each proof is conditional on the source IR's well-formedness proof. A compiler
error need not prove that no translation exists, but it must not produce a
partially verified target.

### Proof-Friendly Production Architecture

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| IR constructors correspond to decidable formal predicates | `rumoca-ir-*` | Bridges Rust and logic |
| Final IR values expose no invariant-breaking mutation | `rumoca-ir-*` | Proofs survive construction |
| Phase inputs and outputs use distinct opaque types | phase boundaries | Theorems match real ordering |
| Phase algorithms are deterministic for fixed inputs | `rumoca-phase-*` | Reproducible proof relation |
| Semantic rewrites use explicit local operations | transformation APIs | Each operation gets one lemma |
| Generated identities retain typed origins | all lowering phases | Auxiliary projection is definable |
| IDs are dense typed ordinals, not text identity | IR arenas | Formal finite maps stay simple |
| Iteration and serialization order are canonical | IR and wire | Avoids permutation obligations |
| Semantic behavior is independent of provenance | formal semantics | Diagnostics do not alter meaning |
| Provenance is total in a separate theorem | IR construction | Traceability remains checkable |
| Validators do not define successful phase states | phase APIs | Proof is established at construction |
| Optimizations have witnesses or are outside verified mode | structural/codegen | Complex search stays untrusted |

Branded Rust lifetimes correspond to finite-index membership predicates in the
formal model. The lifetime mechanism need not be modeled as dynamic behavior;
its constructors must correspond to the formal ownership relation.

### Implementation Strategy

The preferred initial architecture is a certifying compiler:

```text
Rust phase:       input -> output + certificate
verified checker: input + output + certificate -> accept | reject
```

These signatures are architectural notation, not current Rust APIs.

| Strategy | Use | Why |
|---|---|---|
| Direct functional proof | Small constructors and evaluators | Logic mirrors implementation |
| Proof certificate | Resolution and structural algorithms | Search remains fast Rust |
| Translation validation | Optimization and code generation | Checks each produced artifact |
| Conventional tests | Semantics/specification validation | Tests do not replace proofs |

Every counterexample fix identifies a phase-local refinement obligation. Its
regression asserts a model-independent transition, construction, or commutation
property reusable by the formal checker or Kani harness. Compiler and runtime
behavior must not branch on model identity, OMC output, or comparison bands.

Certificates are phase results or ephemeral artifacts, not mutable IR fields.
The checker is substantially smaller than the producing algorithm and has a
machine-checked soundness theorem.

### Structural and Numerical Boundaries

| Transformation | Required evidence | Key assumptions |
|---|---|---|
| Alias elimination | Substitution and reconstruction map | Expression equality |
| BLT ordering | Permutation and dependency witness | Complete incidence relation |
| Matching | Checked matching certificate | Required cardinality |
| Tearing | Retained-variable reconstruction | Solvability conditions |
| Index reduction | Differentiation/refinement derivation | Regularity and differentiability |
| Scalarization | Domain-to-row bijection | Shape and domain agreement |
| Simplification | Local expression equality proof | Operator side conditions |

Symbolic compilation is first interpreted over exact mathematical values.
Floating-point execution is a later refinement with explicit discretization,
roundoff, convergence, event-localization, and external-function assumptions.
Numerical tolerance is not used to excuse an incorrect symbolic rewrite.

### Assurance Evidence Objective

The intended operational goal is to make a Rumoca code-generation use
straightforward to assess within an airborne or automotive software assurance
case. Formal verification supplies evidence; it does not itself certify a
product, assign a software level, qualify a tool, or replace authority and
domain-expert review.

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Each codegen use declares its assurance profile | project configuration | Qualification depends on use |
| The profile closes the accepted Modelica feature set | compiler entry | Coverage cannot be implicit |
| Target language and runtime assumptions are versioned | codegen manifest | Generated behavior needs context |
| Every output maps to source requirements and spans | evidence bundle | Review needs bidirectional traceability |
| Every phase records deterministic input/output identities | evidence bundle | Artifacts must be reproducible |
| Proof and certificate status is reported per phase | evidence bundle | Missing evidence stays visible |
| Unsupported or unproved features fail closed | verified profile | No silent assurance downgrade |
| Generated code is reviewable without compiler internals | target templates | Independent verification remains possible |
| Resource and numeric bounds are emitted explicitly | target evidence | Safety analyses need limits |
| Tool version, configuration, and dependencies are pinned | build manifest | Qualification applies to a baseline |
| Evidence formats are stable, documented, and machine-readable | assurance tooling | Audits must be repeatable |
| Certification claims require qualified expert approval | release process | Architecture cannot grant approval |

The future evidence bundle should include:

| Artifact | Purpose |
|---|---|
| Source and requirements trace map | Connect requirements, model elements, IR, and code |
| MLS formalization manifest | Map every SPEC_0022 contract to proof status |
| Modelica Association gap tracker | Preserve clauses, counterexamples, and proposed wording |
| Phase refinement report | Identify theorem or accepted certificate for every phase |
| Deterministic build manifest | Pin sources, options, tools, templates, and dependency hashes |
| Generated-source map | Map target statements to typed IR and source provenance |
| Assumption register | Record numerics, runtime, external functions, compiler, and hardware |
| Resource report | Bound stack, memory, iteration, timing inputs, and generated storage |
| Verification results | Record tests, coverage, analysis, and independent checks |
| Change-impact graph | Restrict re-verification using semantic dependencies |
| Tool operational requirements | Define intended use and detected misuse |

Assurance mappings to DO-178C/DO-330/DO-331/DO-333 or ISO 26262 are
use-specific; Rumoca assigns no qualification or integrity level.

### Trusted Computing Base

| Component | Required treatment | Why |
|---|---|---|
| Proof-assistant kernel | Explicit trusted version | Checks all theorems |
| Formal Modelica semantics | Reviewed definition | Defines source correctness |
| Verified checker/extracted phase | Machine-checked implementation | Connects artifacts to proofs |
| Rust compiler and linker | Trusted or translation-validated | Connects source to binary |
| Target/runtime semantics | Formal definition or contract | Defines observations |
| Numerical solver | Verified, bounded, or assumed | Controls trajectory claims |
| External functions | Typed contracts or exclusion | Arbitrary code is not proven |
| Hardware floating point | Stated model | Bounds machine arithmetic |

Every public verification report names the verified profile, target, theorem,
tool versions, and remaining trusted assumptions.

### Adoption Sequence

| Milestone | Scope | Exit evidence |
|---|---|---|
| V1 | Scalar continuous, event-free Flat to exact Solve | Machine-checked residual equivalence |
| V2 | Initialization and discrete events | Superdense trace refinement |
| V3 | Symbolic arrays and structured families | Domain-preservation proofs |
| V4 | Clocks, delay, and temporal state | Clock/history refinement |
| V5 | Structural transformations | Checked transformation certificates |
| V6 | One generated target | Translation-validation theorem |
| V7 | Numerical runtime profile | Published error-bound theorem |

OMC/MSL validation and counterexample handling follow
[SPEC_0033 §6a](SPEC_0033_DEVELOPMENT_PROCESS.md#6a-two-tier-verification-cadence);
they are not proof evidence.

### Phasing

Each wave is additive: a wave adds evidence, and none of them upgrades a claim
about the compiler on its own.

| Wave | Scope | Exit evidence |
|---|---|---|
| W1 | Bounded-verification harnesses over the stable kernels, and slice 1 of an executable reference semantics for the discrete/event core | Harness properties stated and running; reference differentially validated against the compiler on hand-written and generated models; every disagreement fixed or recorded |
| W2 | Reference slices 2–4 (continuous coupling, clocked partitions, arrays) | Differential agreement across the wider fragment |
| W3 | Proof assistant selected by maintainer vote; one phase theorem checked | Reproducible proof build; bounded proof CI |

W1 harnesses are written so a Kani proof and a property-test fallback drive the
same property function; the property text is the deliverable, and the driver is
whichever the toolchain supports. A fallback run is validation evidence and
MUST NOT be reported as a proof.

Kani-backed W1 evidence is reproducible only when all of the following are
present in the same revision:

- the Kani release and its Rust toolchain are pinned by the repository;
- `cargo xtask verify kani` is the canonical local and CI entry point;
- CI runs every required harness named by a checked-in proof manifest;
- the Kani 0.67 driver verifies one harness at a time within each deterministic
  manifest stripe, because its parallel
  text stream does not bind each result block to its harness and therefore
  cannot support fail-closed per-harness timing and cover attribution;
- CI MAY run fixed manifest stripes as required matrix jobs, provided the
  stripe function is deterministic and their union is the complete manifest;
- unwinding checks remain enabled and incomplete proofs fail the command;
- the result records the Kani version, harness name, declared bound, elapsed
  time, and success or failure; and
- the ordinary development shell does not silently substitute a property-test
  fallback when Kani is unavailable.

A proof manifest entry identifies a bounded property, not a compiler-wide
verification claim. Removing or renaming a required harness without updating
its owning specification and manifest is a verification failure.

`rumoca-reference` ([SPEC_0041 §4](SPEC_0041_CRATE_OWNERSHIP_CATALOG.md#4-layering-ownership-catalog-spec_0029-12))
is the W1 executable definition. It is a candidate for the "One IR semantics
implemented" criterion below, not a discharge of it.

## Promotion Criteria

| Requirement | Evidence |
|---|---|
| Proof assistant selected | Recorded evaluation and maintainer vote |
| Verified Modelica profile defined | Versioned semantic feature manifest |
| One IR semantics implemented | Executable formal definition |
| One phase theorem checked | Reproducible proof build |
| Rust/formal correspondence chosen | Direct proof or certificate design |
| Trusted computing base reviewed | Published assumptions |
| Proof CI bounded | Reproducible runtime and resource report |

## References

- [SPEC_0007](SPEC_0007_IR_PIPELINE.md) — production phase contracts.
- [SPEC_0036](SPEC_0036_VALID_BY_CONSTRUCTION_IR.md) — proof-friendly IR construction.
- [SPEC_0041](SPEC_0041_CRATE_OWNERSHIP_CATALOG.md) — ownership row for the reference-semantics crate.
- [SPEC_0028](archive/deferred/SPEC_0028_CERTIFICATION_CODEGEN.md) — deferred safety-oriented code generation.
- [CompCert semantic preservation](https://compcert.org/man/manual001.html)
- [CakeML verified compiler](https://cakeml.org/)
- [seL4 verification](https://sel4.systems/Verification/)
- [FAA AC 20-115D](https://www.faa.gov/airports/resources/advisory_circulars/index.cfm/go/document.information/documentNumber/20-115D)
- [ISO 26262 overview](https://www.iso.org/publication/PUB200262.html)
