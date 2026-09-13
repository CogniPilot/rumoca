# SPEC_0052: Rumoca plus Lean: Roadmap for Formally Verified Cyber-Physical Systems

## Status
PROPOSED

## Summary
Develop a provable link from declarative physics and Lean control algorithms to
executables, supported by checked certificates, counterexample search, and experiments.

## Specification

This is a future roadmap; the capabilities below are proposed milestones, not
claims about the current implementation. Compiler verification continues to use
the checker and semantics architecture of [SPEC_0037](SPEC_0037_FORMALLY_VERIFIED_COMPILER.md).

### 1. Vision

Rumoca is a mechanization engine that transforms declarative physics into
computations while preserving a provable link between high-level meaning and
low-level executables. Lean hosts both the control algorithms and their proofs.
Optimization backends search for counterexamples when proofs do not go through,
and experiments ground the assumptions.

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| State each claim's model, domain, assumptions, and implementation link | proof artifacts | A theorem is meaningful only within its stated scope |
| Separate symbolic preservation, numerical error, and physical model validity | system assurance | Each requires different evidence |
| Use experimental evidence to test assumptions and refine models | experiment harness | Physical validity cannot follow from a formal model alone |

### 2. System split

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Describe the plant and sensing in Modelica | plant and sensor models | Declarative equations retain physical structure |
| Implement control and estimation in Lean alongside their proofs | GNC library | The proved algorithm travels with its implementation |
| Describe task periods and jitter in a small declarative execution model | execution model | Timing becomes an explicit part of system behavior |
| Define shared signal, state, sampling, and scheduling interfaces for simulation and proof | composition boundary | Both views must describe the same composed system |

### 3. IR plan

DAE IR is the semantic truth for the elaborated physical model. Numeric Solve
IR supplies its executable realization under the phase contracts in
[SPEC_0007](SPEC_0007_IR_PIPELINE.md); lowering carries the obligations connecting
that realization to the DAE meaning.

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Preserve the DAE meaning when lowering to numeric Solve IR | DAE-to-Solve lowering | Numerical execution needs an auditable semantic anchor |
| Add Lie group annotations with explicit representation and frame conventions | DAE phase | Geometry must survive elaboration and lowering |
| Represent tangent derivatives with their tangent spaces and chosen trivializations | DAE phase | Ambient array derivatives alone do not specify manifold derivatives |
| Preserve tensor structure under the existing range-preserving contracts | DAE and Solve IR | Geometric structure must compose with tensor-native compilation |
| Emit provenance from flattened and transformed equations back to source components | compiler provenance | Proof obligations and counterexamples must remain interpretable |
| Record numerical and runtime assumptions linking Solve execution to each claim | proof/export boundary | A real-valued theorem alone does not certify floating-point execution |

### 4. GNC in Lean

Start with a small SO(3) attitude estimator and a compatible controller, then
expand the guidance, navigation, and control library as its proof coverage grows.

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Package executable algorithms with their statements and checked proofs | Lean GNC library | Users need the implementation and its applicable guarantee together |
| State sensor, disturbance, initialization, and timing assumptions explicitly | estimator and controller proofs | Stability and estimation guarantees depend on these conditions |
| Connect the implementation used in simulation to the proved definition | Lean backend and composition | An independent rewrite breaks the proof-to-implementation link |

### 5. Certificates

Anyone can propose a certificate; Lean checks it. Store Lyapunov, passivity,
contraction, and reachability certificates with the system artifact they concern.

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Bind each certificate to its model, property, assumptions, and checked proof | certificate store | A certificate must not silently apply to another system |
| Accept certificates through Lean checking, independently of the proposing tool | proof boundary | Search and synthesis need not enter the trusted base |
| On unsuccessful proof attempts, search for counterexamples using optimization backends | prove-or-search loop | Concrete violating candidates can expose missing assumptions or incorrect claims |
| Replay candidate counterexamples in simulation and turn confirmed failures into tests | validation harness | Search results need reproducible evidence |
| Use experiments to investigate physical assumptions exposed by those tests | experiment harness | Model failures can reveal mismatches with the physical system |
| Preserve an inconclusive outcome when neither proof nor counterexample is obtained | result reporting | Failure to prove or find a violation establishes neither conclusion |

### 6. Near-term steps

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Prototype a Lean backend for a declared, small DAE/Solve subset with provenance | backend prototype | A bounded scope makes the semantic connection reviewable |
| Prove one SO(3) estimator result and one controller result with explicit domains | Lean GNC prototype | Concrete examples establish a useful initial proof surface |
| Include a simple RTOS model with task periods and bounded jitter | execution prototype | Scheduling assumptions must participate in composition |
| Demonstrate a checked certificate and an unsuccessful proof followed by search and replay | end-to-end example | The prove-or-search loop needs executable evidence |

## References

- [SPEC_0007](SPEC_0007_IR_PIPELINE.md): existing DAE and Solve phase contracts.
- [SPEC_0032](SPEC_0032_RANGE_PRESERVING_TENSORS.md): tensor representation and lowering.
- [SPEC_0037](SPEC_0037_FORMALLY_VERIFIED_COMPILER.md): checker proofs, reference semantics, and trusted-base accounting.
- [SPEC_0039](SPEC_0039_PROOF_CARRYING_SPARSITY.md): compiler witness and checker precedent.
