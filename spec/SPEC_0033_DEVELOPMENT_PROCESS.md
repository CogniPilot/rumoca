# SPEC_0033: Development Process

## Status
ACCEPTED

## Summary
Development work MUST be grounded in the governing spec/MLS rule, trace
bugs to the first owning layer, and verify the smallest behavior-proving path
before broader review gates.

## Motivation

- Keep semantic fixes spec-backed instead of model-by-model guesswork.
- Prevent downstream symptom patches from hiding upstream compiler bugs.
- Give humans and AI agents one compact workflow contract before PR review.

## Specification

### 1. Applicability

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| This spec is mandatory for non-trivial development work | humans and AI agents | Same process for all contributors |
| Conflicting user instructions MUST be surfaced before proceeding | AI agents | Avoid silent policy bypass |
| PR finalization still follows SPEC_0025 | all PRs | Review policy stays separate |

### 2. Spec-First Triage

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Semantic changes MUST cite governing MLS and Rumoca spec sections before editing | compiler/simulator changes | Semantics need normative anchors |
| Non-trivial bugs MUST start from one concrete reproduction | bug triage | Surface symptoms are hypotheses |
| Triage MUST identify the first phase where actual behavior diverges | parse through runtime | Fix the producer, not fallout |
| Triage MUST reject plausible competing hypotheses with evidence | bug triage | Prevents speculative fixes |
| Fixes SHOULD land at the earliest responsible layer | owning crate/phase | Preserves upstream invariants |
| Later-layer fixes MUST justify why earlier ownership is infeasible | validators/runtime/templates | Avoids compatibility workarounds |

Preferred ownership order for compiler bugs:

1. Parse, resolve, typecheck, instantiate, flatten, or ToDae producer.
2. Compile/session orchestration.
3. Solver/runtime/template layer only for truly downstream issues.

### 3. Evidence Requirements

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Bug explanations MUST include the real failing model/code path | triage notes | Concrete examples focus review |
| Expected and actual behavior MUST be stated plainly | triage notes | Keeps findings understandable |
| Relevant phases MUST be mapped from source to failure | compiler bugs | Shows first divergence |
| Semantic identity MUST be proven from compiler-owned data | names/symbols | Strings are not semantics |
| Namespace aliases and component instances MUST stay distinct unless spec-backed | resolver/flattening | Prevents false symbol merges |
| Before/after artifacts MUST prove producer changes for non-trivial semantic fixes | IR/DAE/trace outputs | Verifies root-cause ownership |

For compiler/simulation triage, prefer flattened, DAE, Solve-IR, OMC
instantiated output, or focused trace artifacts over aggregate pass/fail
counts. Solver failures are upstream suspects until emitted equations,
variable selection, and runtime-bound function names are checked.

### 4. Compatibility And Strictness

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Default behavior MUST remain strict and spec-aligned | compiler/tooling | Avoids silent drift |
| Compatibility deviations MUST be explicit and opt-in | config/tooling | Users choose non-standard behavior |
| Compatibility docs MUST name the requiring library/model and default | deviation docs | Makes exceptions reviewable |
| Validators/checkers MUST NOT be weakened just to pass failing models | validation layers | Hides producer bugs |
| Temporary debug probes MUST be removed before finalization | all changes | Keeps tree clean |

### 5. MSL-Backed Work

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Compiler quality claims SHOULD use MSL evidence when relevant | semantic/sim changes | MSL is the broad corpus |
| Failing models MUST stay visible in validation scope | MSL workflows | No hidden exclusions |
| Failures MUST be classified before policy decisions | triage reports | Separates bugs from non-standard input |
| Focused or partial MSL snapshots MUST NOT be promoted | baseline workflow | Prevents baseline drift |
| Commit-to-commit comparisons MUST use the same focused target list | regression triage | Makes deltas meaningful |

Failure classifications:

| Verdict | Required next step | Brief Justification |
|---|---|---|
| Rumoca bug | Fix earliest owning compiler/runtime layer | Project owns behavior |
| Non-standard library pattern | Keep strict default; add opt-in only if approved | Standards stay default |
| Ambiguous policy decision | Stop and record the decision point | Avoids hidden policy |

### 6. Verification And Done Criteria

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Run the smallest focused check that proves changed behavior first | local workflow | Fast evidence before broad gates |
| Required PR gates are selected by SPEC_0025 | PR workflow | One review source |
| Commands not run MUST be reported with reason | final updates/PRs | Exposes residual risk |
| Work is not done while temporary probes or symptom patches remain | all changes | Prevents cleanup debt |
| Semantic work is done only after spec grounding, root-cause proof, and regression coverage | compiler/simulator | Fix must be defensible |

## References

- [SPEC_0007](SPEC_0007_IR_PIPELINE.md) — compiler phase ownership.
- [SPEC_0022](SPEC_0022_MLS_COMPILER_COMPLIANCE.md) — MLS compliance catalog.
- [SPEC_0025](SPEC_0025_PR_REVIEW_PROCESS.md) — PR review and gate reporting.
- [SPEC_0029](SPEC_0029_CRATE_BOUNDARIES.md) — crate boundary ownership.
