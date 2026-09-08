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

### 2a. Bounded Cuts And Proof Packets

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Before editing, each cut ledger MUST name its invariant, earliest constructor, predecessors, keystone files/type shapes, writer, reviewer, and reservation window. Exemption requires no type-shape, public-item, or normative change and reviewer approval. Mechanically forced consumers MUST be added before editing; other widening is prohibited | development ledger | Make ownership and scope explicit |
| Proof packets MUST record spec/MLS anchors, reproduction, first divergence, rejected hypotheses, producer delta, expressible positive/negative/mutation witnesses, commands/results with exit status, commands not run, per-claim `VERIFIED`/`INFERRED`, and `ACCEPT`/`FIX`. Relays MUST name their source and remain `RELAYED-UNVERIFIED` until independently checked | author/reviewer handoff | Evidence survives session boundaries |
| Read-only audits MAY overlap; writers only on disjoint files and changed type shapes. Earlier ledger declaration wins. A blocking holder MUST checkpoint or release; unreleased reservations lapse at their declared window | concurrent work | Prevent collisions and permanent locks |
| Focused gates, strict scoped Clippy, formatting, and diff check MUST be green before review. Review binds those bytes; later proof-boundary edits invalidate `ACCEPT`. `FIX` MUST cite an executable counterexample, normative violation, or mechanism-proved construction escape. Dependency-closed accepted cuts MUST checkpoint before overlapping work opens | review/checkpoint | Review stable green bytes once |
| Unrelated findings MUST enter the durable backlog unless they invalidate the current invariant. `cargo xtask verify quick` runs at dependency-closed milestones; `cargo xtask verify full` runs on PR-final frozen bytes after quick | roadmap/verification | Bound scope and broad-gate cost |
| Static obligations MUST be classified as construction-time, deferred-construction, or runtime-dependent. Each fact has one issuer at its earliest complete-input phase and opaque consumers. Earlier phases MUST carry a typed outstanding obligation for deferred construction. An absent identity or obligation-free deferral is not proof. Re-proving an already issued fact is prohibited | compiler pipeline | Check each semantic fact once |

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
| Validators/checkers MUST NOT be weakened to pass models. Deletion requires unrepresentability by construction and a mutation witness preserving refusal | validation layers | Hides producer bugs |
| Verification ease is a first-class compiler-design benefit and MAY justify refactoring. Changes MUST preserve required source semantics and construction-safety guarantees. Adversarial review MUST weigh verification gains against maintainability, representation, and performance costs using preservation evidence and measurements, or an explicit mechanism-backed explanation of why cost is unaffected. If costs outweigh verification gains, the review MUST record `FIX`. Successful extraction alone is not preservation evidence | verification-driven development | Balance proof tractability with compiler safety, maintainability, and efficiency |
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
| Trace production, classification, comparison, and malformed-evidence rejection MUST satisfy every row in [SPEC_0050 §§1–4](SPEC_0050_TRACE_EVIDENCE_CATALOG.md) | trace producers/comparator | Detailed rows are normative by reference |
| Parity claims require complete strict-high comparator evidence under SPEC_0050; `sim_ok`, exclusions, non-identifiability, and partial runs are never affirmative parity | reports, PRs, specs | Completion is not correctness |
| Every actionable comparator counterexample blocks breadth/release work and closes only through the general-fix evidence in SPEC_0050 §5 | campaign planning | Wrong output outranks breadth |
| Every quoted parity number obeys SPEC_0050 §6 source/run/count rules; Tier 1 is never a cohort metric | reports, PRs, specs | Claims remain reproducible |

[Canonical commands](SPEC_0050_TRACE_EVIDENCE_CATALOG.md#7-canonical-tier-commands)
duplicate this cadence.

#### Tier 2 parity-number acceptance contract

[SPEC_0050 acceptance rows](SPEC_0050_TRACE_EVIDENCE_CATALOG.md#3-tier-2-parity-number-acceptance-rows)
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

### 6c. Working-Model Proof Admission

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| A model that compiles, simulates, or reaches strict-high trace parity is a proof candidate, not a proved compiler path | reports/campaign planning | Matching output can be vacuous or accidental |
| A proof candidate MUST remain a blocking proof obligation until one checked record accounts for its source-semantic obligations, their intended IR owners, its production-path construction receipt, and every emitted trace coordinate | proof-cohort harness | A green aggregate cannot explain why a model works |
| Source-semantic obligations MUST be derived from compiler-owned typed source facts; observed IR obligations MUST come from construction-issued identities or receipts, never model-name branches, display-name matching, span coincidence, or producer-authored success booleans | compiler/proof harness | Evidence must identify the semantic object it claims to cover |
| An obligation absent from a model is recorded as not exercised and MUST NOT count as proof of that capability | proof-cohort reports | Scalar models cannot certify tensor lowering |
| Every trace channel MUST be partitioned deterministically as compared, shared-but-unmeasured, candidate-only, or reference-only; every non-compared channel requires an explicit typed semantic relation or boundary reason and MUST NOT disappear by filtering or set intersection | trace comparator/proof harness | Omitted observables can hide incorrect or missing results |
| A Rumoca-only coordinate MUST carry a closed compiler-issued origin reason from the phase that created it; an absent reason is unrepresentable, not an optional field attached by the proof harness | IR/trace construction | The compiler owns its generated coordinates and must preserve why they exist |
| A reference-only coordinate blocks admission unless an MLS-grounded review proves it is not a required observable; model-name or channel-name exemption lists are prohibited | proof-cohort review | An external compiler's internal coordinates cannot be classified from spelling |
| A proof admission MUST reject an absent receipt, an empty exercised-obligation set, an unexplained unmatched coordinate, an alternate or repeated semantic lowering, a fallback or repair path, a stale artifact identity, and any unsupported construct not closed by an early typed refusal | proof-cohort harness | Admission is fail-closed and non-vacuous |
| Proof-cohort membership is explicit and reviewed; an incidental new pass remains a candidate and MUST NOT be admitted automatically | checked-in proof manifest | Breadth changes cannot silently redefine evidence |
| Each proof-cohort record MUST bind the canonical source and digest, the declared endpoint cone and terminal dispositions, admission status, last compliance-review instant, reviewed revision and worktree state, the normalized compiler-production footprint and its digest, and durable review and evidence references; the checker recomputes every content digest and rejects an absent or unequal binding | checked-in proof manifest | A timestamp, model name, and green test cannot identify the compiler bytes that were reviewed |
| A golden-coverage cone MUST be declared before measurement; coverage is discovery and scope evidence that may falsify the cone but MUST NOT generate, widen, or prove it | proof-cohort harness | A cone inferred from its tests is tautological |
| Golden coverage is the union of instrumentable first-party production source lines executed by exact, isolated scenarios for admitted models divided by the complete instrumentable first-party production-line denominator; candidate, rejected, skipped, or failed scenarios contribute zero to that union | coverage harness | The metric names reviewed compiler code, not incidental green code |
| Golden-coverage reports MUST publish each model's covered-line numerator, the cumulative union, marginal contributions, the denominator count and delta, and exact source digests; overlap counts once and denominator shrinkage remains visible | coverage harness | Coverage growth and code deletion are different events |
| Scenario execution MUST run only the manifest-declared exact test identities; whole-crate or substring-filtered tests MUST NOT contribute to a model footprint | coverage harness | Unrelated tests would silently inflate the reviewed path |
| The checker MUST reject an instrumentable semantic owner executed outside the declared cone and a declared instrumentable owner not executed; declarative, compile-time-only, and generated-artifact owners require explicit non-runtime dispositions and separate evidence | proof-cohort checker | Bidirectional closure rejects both hidden execution and vacuous declarations |
| A generic or macro-attributed source line proves only the exact instantiation or expansion identified by the coverage tool; when a stable discriminator is unavailable, that line MUST NOT be admitted as golden-covered semantic code | coverage harness | One invocation must not launder every generated implementation |
| Compiler source coverage proves that emission ran, not that emitted artifacts executed; every executable emitted endpoint requires a separately named generated-artifact coverage and behavior rung | proof-cohort harness | Green emitter lines cannot certify dead or bypassed generated code |
| Campaign breadth MUST remain frozen while a working-model candidate has an unresolved proof obligation, unless the maintainer explicitly records a superseding campaign decision | campaign planning | Depth-first proof precedes breadth |

The proof record explains *why* each admitted model works. It is not another
green Boolean. At minimum it binds the exact source and compiler artifacts,
the sole phase-product chain, the exercised and unexercised obligations, the
checked source-to-IR receipt, complete channel accounting, strict-high trace
evidence when the model is simulatable, and the independent checker result.
The checker consumes evidence; it does not trust a producer's claim that the
evidence is complete.

Golden coverage is ordinary source-line coverage over those admitted model
scenarios, not a language-feature score. For example, a model that is correctly
refused by the eFMI capability gate contributes the executed gate and diagnostic
lines, but contributes no coverage for the GALEC projection it never enters.

## References

- [SPEC_0007](SPEC_0007_IR_PIPELINE.md) — compiler phase ownership.
- [SPEC_0022](SPEC_0022_MLS_COMPILER_COMPLIANCE.md) — MLS compliance catalog.
- [SPEC_0025](SPEC_0025_PR_REVIEW_PROCESS.md) — PR review and gate reporting.
- [SPEC_0029](SPEC_0029_CRATE_BOUNDARIES.md) — crate boundary ownership.
- [SPEC_0050](SPEC_0050_TRACE_EVIDENCE_CATALOG.md) — normative trace-evidence
  catalog bound by §6a.
- [SPEC_0052](SPEC_0052_EMBEDDED_C_COMPETITOR_MATRIX_CATALOG.md) — normative
  embedded-C competitor catalog bound by §6b.
