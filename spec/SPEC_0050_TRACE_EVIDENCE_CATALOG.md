# SPEC_0050: Trace Evidence Catalog

## Status
REFERENCE

## Summary
Lookup catalog for trace production, rejection, and acceptance governed by
SPEC_0033 §6a.

## Specification

Rows below carry the lookup/evidence detail bound by SPEC_0033 §6a, are
normative by reference, and add no independent requirement. Canonical commands
carry that governing cadence without creating another rule source.

### 1. Trace Production and Rejection Rows

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Produced traces MUST have finite nondecreasing time, unique channels, and rectangular data | trace producers | Interpolation needs a valid relation |
| A time regression MUST fail unless the preceding row is settled and the shared predicate proves one semantic instant; retain that settled row unchanged | trace producers | Proximity alone cannot hide lateness |
| At one coordinate, settled replaces initialization, event-left, or nominal; event-left never replaces settled; exact duplicate nominal suppresses without reevaluation | trace producers | Preserve superdense role order |
| Published state events MUST use the common host application coordinate; localization and continuation coordinates stay private | ME host | Solvers cannot change trace semantics |
| The comparator MUST reject malformed time, names, or shape before interpolation and MUST NOT repair evidence | trace comparator | Oracles cannot manufacture evidence |

### 2. Comparison And Classification Rows

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| A parity claim MUST come from OMC agreement bands; `sim_ok` is completion only | reports, PRs, specs | An unchecked trace may be wrong |
| Initialization parity uses each trace's last row at the exact common start time | comparator | Positive time is trajectory |
| Every `sim_ok` trace is a candidate and is compared or receives a typed `skipped`, `missing_trace`, or `trace_nonidentifiable` reason | comparator | Every completion is accounted for |
| `trace_nonidentifiable` is separate, outside the pointwise denominator, and never strict-high, passing, supported, or certified | all consumers | Inapplicability is not evidence |
| Stochastic classification follows typed random IR; deterministic-chaotic evidence records a positive finite Lyapunov lower bound, sample count, and artifact digest | trace producer | Classification is machine-readable |
| Non-identifiability profiles list every outstanding proof obligation; incomplete evidence fails | producer/comparator | Classification proves no result |
| Classification MUST NOT branch on model name, OMC output, or observed band | producer/comparator | No corpus exceptions |
| Tracked exclusions explain why pointwise comparison is non-identifying and remain visible, non-strict-high, and non-counterexamples | comparator | Oracle boundaries stay auditable |
| Pointwise-nonidentifiable traces and oracle limitations are tallied separately as non-strict-high | reports/evidence | Separation changes no verdict |
| Outcome-reading validity checks run only after comparison | harness flow | Early abort destroys evidence |
| Comparison evidence MUST retain the deterministic partition of unique channel names into compared, shared-but-unmeasured, Rumoca-only, and reference-only sets, including in a no-common or no-comparable failure | comparator output | Filtering and set intersection must not erase the scope of the claim |
| Non-compared channels do not change a numerical agreement band by themselves, but a SPEC_0033 §6c proof admission MUST account for each through a compiler-owned semantic relation or typed boundary reason | proof-cohort harness | Numerical parity and complete-model proof are distinct claims |

### 3. Tier 2 Parity-Number Acceptance Rows

| Requirement | Enforced by |
|---|---|
| Comparator ran, or shard bands were merged | `MslParityStageOutcome` |
| Reference exists and names `omc_version` | `MslParityMeasurement::measured` |
| Trace comparison exists and is non-empty | `check_comparator_evidence` |
| `models_compared > 0`; otherwise report `parity unmeasured` and fail the quality gate | `MslParityMeasurement::measured` |
| Every `sim_ok` is compared or has a typed boundary | `quantify_trace_differences` |
| Band table has one row per target and binds its comparator output | `band_table::ensure_comparable` |
| Table counts and digest match its rows | `band_table::ensure_comparable` |
| Table bands equal the reference bands | `band_table_disagreement` |
| Full cohort can identify every baseline-certified strict-high model | `certified_cohort_regression_reasons` |
| No baseline-certified strict-high model departed or changed band | `certified_model_regression` |
| Reference `total_models` equals `sim_target_models` | `load_current_msl_parity_gate_input_required` |
| Reference has no OMC assertion failures | `load_current_msl_parity_gate_input_required` |

### 4. Strict-High Claim Rows

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| The cohort ratchet is strict-high with zero deviation channels | harness | One wrong observable falsifies parity |
| `sim_ok` remains a raw execution count, never supported/certified/passing | all reports | Completion proves no semantics |
| Package/stage simulation pass requires comparable strict-high OMC trace | package report | Near or absent is unsupported |
| Every full-run `sim_ok` is strict-high, tracked exclusion, or typed non-identifiable | harness | Complete classification |
| Every unbounded non-high result is a counterexample or harness defect | review evidence | Wrong traces falsify claims |

### 5. Counterexample Closure Rows

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| A counterexample yields a general semantic fix or typed profile rejection | compiler/runtime | No model exception |
| Any actionable counterexample blocks merge, release, and unrelated capability work until zero | campaign | Correctness precedes breadth |
| Closure is per model: strict-high, typed refusal, or reasoned exclusion | review evidence | Aggregates cannot hide defects |
| Closed defects retain a focused regression and the model remains in the next Tier 2 run | tests/cohort | Repairs stay exercised |
| Tolerance changes, retries, and model-specific branches cannot close a counterexample | implementation | Exceptions prove nothing |

### 6. Quoted-Number Source Rows

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| A quoted number states `models_compared` plus skipped/missing counts | reports | Coverage accompanies result |
| Every quoted number names its Tier 2 run and commit | reports | Result is reproducible |
| Partial, focused, single-shard, or stale runs never supply parity numbers | all claims | Partial data is not cohort evidence |
| Tier 1 deltas are never cohort parity numbers | review evidence | Canary is a tripwire |

### 7. Canonical Tier Commands

```bash
# Tier 1 — fixed 20-model canary; the harness marks this snapshot partial.
CARGO_BUILD_JOBS=4 RUST_TEST_THREADS=4 RAYON_NUM_THREADS=4 cargo xtask verify msl-parity \
  --sim-targets-file infra/verification/msl-canary-20.json

# Tier 2 — full cohort; CI shards it as `--shard m/n` plus `--merge-shards DIR`.
CARGO_BUILD_JOBS=4 RUST_TEST_THREADS=4 RAYON_NUM_THREADS=4 cargo xtask verify msl-parity
```

## References

- [SPEC_0033 §6a](SPEC_0033_DEVELOPMENT_PROCESS.md#6a-two-tier-verification-cadence)
  — owning development-process rules.
