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
| Compiler-owned IRs, wire formats, and phase APIs MUST support only their current representation | compiler | Old internal contracts must not constrain correct architecture |
| A compiler representation cutover MUST remove the superseded reader, writer, adapter, alias, feature flag, fixture, and fallback branch in the same change | compiler | Prevents obsolete paths from bypassing current invariants |
| Unsupported compiler-owned wire versions MUST fail immediately | IR deserialization | Invalid input must not enter the pipeline |
| Source-language compatibility deviations MUST be explicit and opt-in | config/tooling | Users choose non-standard Modelica behavior |
| Source-language compatibility docs MUST name the requiring library/model and default | deviation docs | Makes exceptions reviewable |
| Validators/checkers MUST NOT be weakened just to pass failing models | validation layers | Hides producer bugs |
| Temporary debug probes MUST be removed before finalization | all changes | Keeps tree clean |

“Compatibility” in the source-language rows concerns intentionally
non-standard source behavior. It does not authorize support for superseded
compiler-owned representations.

### 5. MSL-Backed Work

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Compiler quality claims SHOULD use MSL evidence when relevant | semantic/sim changes | MSL is the broad corpus |
| Failing models MUST stay visible in validation scope | MSL workflows | No hidden exclusions |
| Failures MUST be classified before policy decisions | triage reports | Separates bugs from non-standard input |
| Focused or partial MSL snapshots MUST NOT be promoted | baseline workflow | Prevents baseline drift |
| Commit-to-commit comparisons MUST use the same focused target list | regression triage | Makes deltas meaningful |
| External compatibility corpora MUST pin an immutable revision and run as bounded parallel gates | CI workflows | Keeps evidence reproducible without creating a serial CI long pole |

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
| Capability changes follow the §6a two-tier cadence | local workflow | Focused proof and cohort proof are different claims |
| Required PR gates are selected by SPEC_0025 | PR workflow | One review source |
| Commands not run MUST be reported with reason | final updates/PRs | Exposes residual risk |
| Work is not done while temporary probes or symptom patches remain | all changes | Prevents cleanup debt |
| Semantic work is done only after spec grounding, root-cause proof, and regression coverage | compiler/simulator | Fix must be defensible |
| Cargo subprocesses launched by repository tooling MUST derive a host-topology job budget for `CARGO_BUILD_JOBS` and `RAYON_NUM_THREADS` unless the caller sets them explicitly | developer tooling | Keeps verification responsive without overriding an operator choice; an underived rayon pool fans out to every CPU inside an already-capped child |
| The automatic Cargo budget MUST reserve zero physical cores below 4 logical CPUs, one below 8, and at most two at 8 or more | developer tooling | Small runners retain throughput while developer machines retain foreground capacity |
| Long-running isolated workers MUST exit when their parent control channel closes and MUST enforce a bounded resident-memory policy | worker orchestration | Interrupted gates must not leave orphaned or unbounded processes |

### 6a. Two-Tier Verification Cadence

| Tier | Cadence | Required evidence |
|---|---|---|
| 1 — focused + canary | Every capability change | Focused suites green, plus a canary delta in the working ledger |
| 2 — cohort sweep | Every milestone, or nightly CI shards | One complete 566-model MSL/OMC sweep at a named commit |

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Verification commands MUST run under `CARGO_BUILD_JOBS=4 RUST_TEST_THREADS=4 RAYON_NUM_THREADS=4` | local and agent workflows | Fixed budget keeps concurrent workers from oversubscribing the host; naming only the first two leaves the derived rayon pool free to reclaim every core |
| A capability change is complete only with Tier 1 focused suites green | all capability work | Focused proof precedes every broader claim |
| A capability change is complete only with its Tier 1 canary delta recorded in the working `dev/` ledger | dev/ ledger | Deltas must outlive the session that produced them |
| The canary target set is the fixed 20-model list in `dev/msl-canary-20.json` | canary runs | A moving target set makes deltas meaningless |
| Replacing a canary member MUST record the rationale and replacement in the same ledger entry | dev/ ledger | Keeps the fixed list auditable |
| Each canary model and phase gets exactly one attempt at the harness default 10-second budget | canary runs | One honest attempt, no retry or default path |
| A canary timeout, panic, unsupported operation, or non-finite result MUST be recorded as a failure | canary runs | Retries and fallbacks manufacture passes |
| Tier 2 MUST cover the full 566-model set, either in one run or as CI shards merged by the fan-in job | CI / milestone | Cohort evidence without a serial CI long pole |
| Tier 2 is the sole source of cohort parity claims | reports, PRs, specs | One cohort number, one origin |
| A parity claim MUST come from the OMC trace comparator's agreement bands; `sim_ok` alone is completion, never parity | reports, PRs, specs | A trace nobody compared can be plausibly wrong |
| A Tier 2 run whose comparator stage did not execute over every `sim_ok` trace reports "parity unmeasured", not a number | reports, dev/ ledger | Missing comparison must be visible, not defaulted |
| An unmeasured cohort run MUST fail its quality gate, not pass with `sim_ok` | harness gate, `verify msl-parity` | A run nobody could check must not read as a green run |
| The cohort ratchet MUST be the strict-high agreement band; `sim_ok` is reported and never gated on | harness gate | Gating completion rewards traces nobody compared |
| No validity check that reads simulation outcomes may run before the comparator stage | harness gate flow | A gate that aborts first destroys the measurement it judges |
| Every quoted parity number MUST name the Tier 2 run and commit it came from | reports, PRs, specs | An unsourced number cannot be rechecked |
| Parity numbers MUST NOT be quoted from a partial, single-shard, focused, or stale run | any claim | Partial snapshots are not cohort evidence |
| A Tier 1 canary delta MUST NOT be reported as a cohort parity number | dev/ ledger, PR text | Tier 1 is a tripwire, not a metric |

```bash
# Tier 1 — fixed 20-model canary; the harness marks this snapshot partial.
CARGO_BUILD_JOBS=4 RUST_TEST_THREADS=4 RAYON_NUM_THREADS=4 cargo xtask verify msl-parity \
  --sim-targets-file dev/msl-canary-20.json

# Tier 2 — full cohort; CI shards it as `--shard m/n` plus `--merge-shards DIR`.
CARGO_BUILD_JOBS=4 RUST_TEST_THREADS=4 RAYON_NUM_THREADS=4 cargo xtask verify msl-parity
```

#### Acceptance contract: what a Tier 2 run must contain to emit a parity number

A run emits a parity number only when ALL of the following are true. Anything
less is reported as `parity unmeasured: comparator did not run`, and the
quality gate fails rather than falling back to `sim_ok`.

| Requirement | Enforced by |
|---|---|
| The comparator stage executed, or its bands were merged from shards that ran it | `MslParityStageOutcome` (`Ran` / `MergedShardArtifacts`) |
| `omc_simulation_reference.json` exists in the results directory and carries `omc_version` | `MslParityMeasurement::measured`, `check_comparator_evidence` |
| `sim_trace_comparison.json` exists and is non-empty | `check_comparator_evidence` |
| `trace_comparison.models_compared > 0` | `MslParityMeasurement::measured`, `check_comparator_evidence`, nightly `Verify the merged sweep measured parity` |
| The reference's `total_models` equals the run's `sim_target_models` (not stale) | `load_current_msl_parity_gate_input_required` |
| The reference records no OMC Modelica assertion failures | `load_current_msl_parity_gate_input_required` |

The quoted number is the **strict-high agreement band** over the cohort target
count. `agreement_minor` and `agreement_deviation` are reported alongside it and
are not part of the number. `sim_ok` is printed in every summary, labelled as
completion, and is gated on nowhere: the cohort ratchet reads
`agreement_high`, and the structural floor reads `agreement_high` as well,
because `agreement_high <= models_compared <= sim_ok` makes a band floor
strictly tighter than the `sim_ok` floor it replaced.

Ordering is part of the contract: the only validity check that may run before
the comparator is "is this run measurable at all" (nonzero discovered models,
no resolve errors). Every verdict that reads simulation outcomes runs after the
comparator stage, so no gate can abort a run before its parity is measured.

**Why:** a Tier 1 run writes `run_scope: "partial"` into its quality snapshot
(`balance_pipeline_quality_gate.rs`), and only a `"full"` scope satisfies the
baseline ratchet. The cadence makes that mechanical distinction a reporting
rule: Tier 1 detects regressions early, Tier 2 states where the cohort stands.
`dev/` is an untracked working ledger, so Tier 1 evidence is developer-local
and is rechecked by rerunning the command above; Tier 2 evidence is a tracked
CI artifact. That asymmetry is the reason only Tier 2 backs a published number.

## References

- [SPEC_0007](SPEC_0007_IR_PIPELINE.md) — compiler phase ownership.
- [SPEC_0022](SPEC_0022_MLS_COMPILER_COMPLIANCE.md) — MLS compliance catalog.
- [SPEC_0025](SPEC_0025_PR_REVIEW_PROCESS.md) — PR review and gate reporting.
- [SPEC_0029](SPEC_0029_CRATE_BOUNDARIES.md) — crate boundary ownership.
