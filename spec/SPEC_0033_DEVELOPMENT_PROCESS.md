# SPEC_0033: Development Process

## Status
ACCEPTED

## Summary
Development MUST follow specs, fix first divergence, and bind claims to closed
evidence.

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

### 3. Evidence Requirements

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Bug explanations MUST include the real failing model/code path | triage notes | Concrete examples focus review |
| Expected and actual behavior MUST be stated plainly | triage notes | Keeps findings understandable |
| Relevant phases MUST be mapped from source to failure | compiler bugs | Shows first divergence |
| Semantic identity MUST be proven from compiler-owned data | names/symbols | Strings are not semantics |
| Namespace aliases and component instances MUST stay distinct unless spec-backed | resolver/flattening | Prevents false symbol merges |
| Before/after artifacts MUST prove producer changes for non-trivial semantic fixes | IR/DAE/trace outputs | Verifies root-cause ownership |
| Covered artifact timestamps and identities MUST be pure functions of one explicit artifact-session input containing a canonical generation instant and identity seed. The artifact path MUST NOT read an ambient clock, RNG, or environment fallback; an interactive entry layer supplies fresh explicit inputs, while reproducible evidence pins them and exercises the identical session path | artifact construction and evidence harnesses | Production and reproducible builds share one byte-identical construction path, so an authenticated package can be regenerated rather than normalized after emission |

One invocation constructs exactly one private, non-cloneable artifact session
after the checked target identity and complete logical file-ID catalog exist;
renderers and package/checksum assembly borrow that session and cannot replace
it. The explicit generation instant has exactly the UTC-second spelling
`YYYY-MM-DDTHH:MM:SSZ`; the identity seed has exactly the lowercase hyphenated
UUID spelling. Each artifact UUID is UUIDv5 under that seed. Its name is the
concatenation of four frames, each encoded as an unsigned 64-bit big-endian
byte length followed by the exact bytes: fixed algorithm tag
`rumoca-artifact-identity-v1`, target-scope kind, target-scope value, and the
manifest-issued local file ID. Built-ins use scope kind
`builtin-registry-key` and their unique registry key; directory targets use
scope kind `canonical-manifest-blake3` and the digest of checked canonical
manifest facts. Display names and raw TOML formatting never enter identity.

### 4. Compatibility And Strictness

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Default behavior MUST remain strict and spec-aligned | compiler/tooling | Avoids silent drift |
| Compiler-owned IRs, wire formats, and phase APIs MUST support only their current representation | compiler | Old internal contracts must not constrain correct architecture |
| A compiler representation cutover MUST remove the superseded reader, writer, adapter, alias, feature flag, fixture, and fallback branch in the same change | compiler | Prevents obsolete paths from bypassing current invariants |
| Language constructs that the Modelica Language Specification itself deprecates remain current user-facing source semantics when SPEC_0022 requires them; they MUST NOT be classified as internal legacy or compatibility code | compiler front end | Supporting a required language construct neither excuses compiler cruft nor permits an internal compatibility path |
| Unsupported compiler-owned wire versions MUST fail immediately | IR deserialization | Invalid input must not enter the pipeline |
| Source-language compatibility deviations MUST be explicit and opt-in | config/tooling | Users choose non-standard Modelica behavior |
| Source-language compatibility docs MUST name the requiring library/model and default | deviation docs | Makes exceptions reviewable |
| Validators/checkers MUST NOT be weakened just to pass failing models | validation layers | Hides producer bugs |
| Hand-written compiler code MUST NOT suppress the Rust `dead_code` lint; unreachable items MUST be deleted and reintroduced only with their first real consumer. Generated parser output with explicit generator provenance is the sole carve-out | compiler crates and architecture gate | A lint allowance can preserve abandoned APIs, speculative helpers, and marker methods outside the current construction chain |
| Temporary debug probes MUST be removed before finalization | all changes | Keeps tree clean |

### 5. MSL-Backed Work

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Compiler quality claims SHOULD use MSL evidence when relevant | semantic/sim changes | MSL is the broad corpus |
| Failing models MUST stay visible in validation scope | MSL workflows | No hidden exclusions |
| Failures MUST be classified before policy decisions | triage reports | Separates bugs from non-standard input |
| Focused or partial MSL snapshots MUST NOT be promoted | baseline workflow | Prevents baseline drift |
| Commit-to-commit comparisons MUST use the same focused target list | regression triage | Makes deltas meaningful |
| External compatibility corpora MUST pin an immutable revision and run as bounded parallel gates | CI workflows | Keeps evidence reproducible without creating a serial CI long pole |
| A compiler- or runtime-contract cutover MAY reset incomparable floors only through a one-shot migration naming both contracts, evidence commit, exact stage counts, target count, and post-cutover failure and diagnostic censuses; the resolver MUST fail closed on any mismatch | baseline workflow | A stricter contract can change classification, but an unaudited reset could hide regression |
| Post-cutover ratchets MUST use the reviewed floor; migration cannot excuse later regressions | baseline workflow | Waivers are one-shot |
| A comparator-policy change MUST increment the gate schema and pin old/new strict-high counts, reviewed-boundary count, and exclusion-artifact digest; unrelated ratchets MUST remain monotonic | baseline workflow | An oracle-boundary change alters the denominator; an unaudited reset could hide regression or counterexamples |

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
| Built-in targets MUST satisfy SPEC_0007's product contract | code generation | Excludes placeholders and lossy output |
| Repository-launched Cargo MUST derive `CARGO_BUILD_JOBS` and `RAYON_NUM_THREADS` from host topology unless explicitly set | developer tooling | Avoids nested oversubscription |
| Automatic Cargo budgets MUST reserve zero physical cores below 4 logical CPUs, one below 8, and at most two otherwise | developer tooling | Balance runner throughput and foreground capacity |
| Long-running isolated workers MUST exit when their parent control channel closes and MUST enforce a bounded resident-memory policy | worker orchestration | Interrupted gates must not leave orphaned or unbounded processes |
| Nix MUST remain optional; shells only provision prerequisites and exclude first-party outputs by default | developer tooling | Cargo/xtask remains canonical |

### 6a. Two-Tier Verification Cadence

| Tier | Cadence | Required evidence |
|---|---|---|
| 1 — focused + canary | Every capability change | Focused suites green, plus a canary delta in the working ledger |
| 2 — cohort sweep | Every milestone, or nightly CI shards | One complete 566-model MSL/OMC sweep at a named commit |

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Verification MUST use `CARGO_BUILD_JOBS=4 RUST_TEST_THREADS=4 RAYON_NUM_THREADS=4` | local and agent workflows | Fixed concurrency budget |
| A capability change is complete only with Tier 1 focused suites green | all capability work | Focused proof precedes every broader claim |
| A capability change is complete only with its Tier 1 canary delta recorded in durable review evidence | PR / verification record | Deltas must outlive the session that produced them |
| The canary target set is the fixed 20-model list in `infra/verification/msl-canary-20.json` | canary runs | A moving target set makes deltas meaningless |
| Replacing a canary member MUST record the rationale and replacement in the same review evidence | PR / verification record | Keeps the fixed list auditable |
| Each non-simulation canary phase gets one 10-second attempt; `Sim` gets a 14-second parent watchdog around its 12-second solver budget | canary runs | One honest attempt per phase, no retry or default path |
| A canary timeout, panic, unsupported operation, or non-finite result MUST be recorded as a failure | canary runs | Retries and fallbacks manufacture passes |
| Tier 2 MUST cover the full 566-model set, either in one run or as CI shards merged by the fan-in job | CI / milestone | Cohort evidence without a serial CI long pole |
| Tier 2 is the sole source of cohort parity claims | reports, PRs, specs | One cohort number, one origin |
| A parity claim MUST come from the OMC trace comparator's agreement bands; `sim_ok` alone is completion, never parity | reports, PRs, specs | A trace nobody compared can be plausibly wrong |
| Initialization parity MUST use each trace's last row at the exact common start time; nearby positive-time rows remain trajectory behavior | trace comparator | Separates initialization from later events |
| Trace production, publication, and malformed-evidence rejection MUST satisfy [SPEC_0050](SPEC_0050_TRACE_EVIDENCE_CATALOG.md) | trace producers/comparator | Detailed rows remain normative by reference |
| The comparator's candidate set MUST be every `sim_ok` trace | `rumoca_model_is_trace_candidate` | Completion picks candidates; comparison decides parity |
| Every candidate MUST be compared or recorded under `skipped`, `missing_trace`, or `trace_nonidentifiable` with a typed reason | `sim_trace_comparison.json` | An uncompared trace must name the exact proof boundary |
| `trace_nonidentifiable` MUST be reported separately, excluded from the pointwise-comparison denominator, and MUST NOT count as strict-high, passing, supported, or certified | comparator and all consumers | Inapplicable pointwise evidence cannot become affirmative evidence |
| Stochastic non-identifiability MUST follow typed random IR; deterministic-chaotic evidence MUST record a positive finite Lyapunov lower bound, sample count, and artifact digest | trace producer | Classification is machine-readable evidence, not a model-name exception |
| A non-identifiability profile MUST list outstanding proof obligations; incomplete evidence MUST fail comparison | trace producer/comparator | Classification narrows the proof method but never discharges the proof |
| Automatic trace classification MUST NOT branch on model name, OMC output, or an observed comparison band | trace producer/comparator | Corpus-specific heuristics cannot establish correctness |
| A run whose comparator stage did not execute, or compared zero models, reports "parity unmeasured", not a number | harness gate, `verify msl-parity` | Missing comparison must be visible, not defaulted |
| A quoted number MUST state `models_compared` and the skipped/missing counts beside it | reports, review evidence | Partial coverage is part of the claim |
| Tracked comparator exclusions MUST explain why pointwise OMC comparison is non-identifying; they remain visible and non-strict-high but are not refinement counterexamples | comparator | Oracle-test boundaries must be auditable |
| An unmeasured cohort run MUST fail its quality gate, not pass with `sim_ok` | harness gate, `verify msl-parity` | A run nobody could check must not read as a green run |
| The cohort ratchet MUST be strict-high; strict-high MUST contain zero deviation channels | harness gate | One wrong observable falsifies parity |
| `sim_ok` MUST remain a raw execution count and MUST NOT be called supported, certified, or passing | reports, PRs, specs | Solver completion does not prove semantics |
| A package or stage simulation pass MUST require a comparable strict-high OMC trace | package pass-rate report | Near, deviation, and absent bands are unsupported |
| A full Tier 2 gate MUST classify every `sim_ok` as strict-high, tracked exclusion, or typed `trace_nonidentifiable` | harness gate | Every completion needs parity or a reviewed boundary |
| Every non-high result without such a boundary MUST be triaged as a refinement counterexample or harness defect | review evidence, issue/PR | Wrong traces falsify the claim |
| A counterexample MUST yield a general semantic fix or typed profile rejection | compiler/runtime owners | Model exceptions cannot establish correctness |
| Any actionable counterexample blocks merges, releases, and unrelated capability work until its count is zero | campaign planning, PR/release gates | False success outranks breadth |
| The actionable counterexample count MUST be zero before compile-frontier or unrelated capability work resumes | campaign planning, review evidence | False success outranks breadth work |
| Counterexample closure MUST be recorded per model as strict-high, typed refusal, or reasoned comparator exclusion | review evidence, issue/PR | Aggregates cannot close false success |
| Every closed counterexample MUST retain a focused regression for the semantic defect, and the originating model MUST remain in the next complete Tier 2 comparison | focused suites, cohort sweep | A repaired proof obligation must not silently regress or disappear from evidence |
| Tolerance changes, retries, and model-specific compiler/runtime branches MUST NOT close a counterexample | compiler/runtime owners | Exceptions in implementation cannot establish semantic correctness |
| Pointwise-nonidentifiable traces and oracle-test limitations MUST be tallied separately as non-strict-high | comparator reports, review evidence | Separation manufactures neither proof nor falsification |
| No validity check that reads simulation outcomes may run before the comparator stage | harness gate flow | A gate that aborts first destroys the measurement it judges |
| Every quoted parity number MUST name the Tier 2 run and commit it came from | reports, PRs, specs | An unsourced number cannot be rechecked |
| Parity numbers MUST NOT be quoted from a partial, single-shard, focused, or stale run | any claim | Partial snapshots are not cohort evidence |
| A Tier 1 canary delta MUST NOT be reported as a cohort parity number | review evidence, PR text | Tier 1 is a tripwire, not a metric |

[Canonical commands](SPEC_0050_TRACE_EVIDENCE_CATALOG.md#3-canonical-tier-commands)
duplicate this cadence.

#### Tier 2 parity-number acceptance contract

[SPEC_0050 acceptance rows](SPEC_0050_TRACE_EVIDENCE_CATALOG.md#2-tier-2-parity-number-acceptance-rows)
are mandatory; omission reports `parity unmeasured`.

### 6b. Authenticated Embedded-C Competitor Matrix

[SPEC_0052 rows](SPEC_0052_EMBEDDED_C_COMPETITOR_MATRIX_CATALOG.md) govern
embedded-C claims.

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Rows bind source/digest, relation, competitor/version/backend, settings, scope, oracle, threshold, and closed evidence state; omission/unknown rejects | manifest | Prevent drift |
| Correctness precedes measurement; equality is tie; missing/`PENDING`/skipped/failed/unmeasured/unauthenticated is never a win | harness/reporter | Reject false wins |
| Results are `Incomplete`, `Rejected`, or `ExactPin { delta, outcome }`; only exact pins expose delta/outcome | harness | Invalid combinations are unrepresentable |
| Row identity is immutable; different comparisons need new rows | manifest | Scope results |
| Ratchets pin exact Rumoca measurements; every change needs fall-only promotion; predecessor non-relaxation rejects old counts | harness/reviewer | Audit promotion |
| Immutable protocol and append-only runner history are separate; runner changes cannot authorize semantics | reviewer | Keep maintenance operable |
| Bootstrap requires complete history, global strict-ancestor absence, and exact HEAD bytes at first introduction | harness | Prevent baseline shopping |
| Broad claims require every scoped row `IMPLEMENTED`, oracle-correct, authenticated, and match-or-beat | reviewer | Bound claims |
| Schema 9 is the one-shot authority to delete retired ECM-001 and replace it with pending ECM-003; schema 8 has no compatibility reader or fallback | manifest/harness | Retired products cannot remain evidence |
| ECM-003 MUST expose no baseline, delta, outcome, or artifact history until its shipped eFMU Production C and oracle receipt are re-authenticated from raw bytes | manifest/harness | Prior-row receipts cannot authorize replacement bytes |

## References

- [SPEC_0007](SPEC_0007_IR_PIPELINE.md) — compiler phase ownership.
- [SPEC_0022](SPEC_0022_MLS_COMPILER_COMPLIANCE.md) — MLS compliance catalog.
- [SPEC_0025](SPEC_0025_PR_REVIEW_PROCESS.md) — PR review and gate reporting.
- [SPEC_0029](SPEC_0029_CRATE_BOUNDARIES.md) — crate boundary ownership.
- [SPEC_0050](SPEC_0050_TRACE_EVIDENCE_CATALOG.md) — normative trace-evidence
  catalog bound by §6a.
- [SPEC_0052](SPEC_0052_EMBEDDED_C_COMPETITOR_MATRIX_CATALOG.md) — normative
  embedded-C competitor catalog bound by §6b.
