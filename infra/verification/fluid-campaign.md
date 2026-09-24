# MSL Fluid compilation and trace validation

## Campaign context

The immediate priority is restoring every model in `fluid-recovery-targets.json`
to strict-high parity while preserving all 196 historically high identities. Repairs must
address general compiler or shared Model Exchange behavior. Focused evidence
and the fixed canary precede a complete cohort comparison; a larger diagnostic
budget does not establish recovery. Fluid capability work remains paused until
these preservation losses are recovered.

The subsequent milestone is one unmodified MSL Fluid example,
`Modelica.Fluid.Examples.Tanks.EmptyTanks`, compiling and simulating end to end
with strict-high OMC agreement. Do not expand the Fluid target set before that
milestone. `fluid-first-target.json` pins this target for `cargo xtask` runs.
The selected water package's inherited enthalpy constant is now resolved to
4184 in the retained Flat IR. At `767151bdc`, EmptyTanks advances from ED019
at `tank1.h_start` to ED008 at `tank1.heatTransfer.states.p`; indexed fields
`states[1].p` and `.T` exist, but the vectorized record call remains unresolved.
This is a bounded diagnostic advance, not successful compilation or simulation.

The latest full 566-model run, `tier2-bulk-clear-7e67b9954/`, at clean
`7e67b9954`, has 211 raw completions, 192 compared, all 192 high, 19 tracked
exclusions, and zero missing comparisons. All prior 190 high identities and
208 raw completions are preserved. Engine1b and Fourbar1 recover high parity
in 7.396 and 8.609 seconds after two shared dense Jacobian clears are lowered
to contiguous bulk zeroing. PlanarFourbar remains high in 5.538 seconds;
IMS_Start retains raw execution in 9.796 seconds. The center-tap thyristor
model recovers raw execution in 11.791 seconds, closing its loss against
`2a0ea3aa` while retaining its existing comparator exclusion.
Historical preservation is 192/196 (190/194 original). IMC_YD,
SMPM_VoltageSource, SMR_DOL and UniversalConstraint still time out.
Whole-model compilation classifications are unchanged. An intermediate Solve
count decreases by one: DC_CompareCharacteristics changes from a structural
error at 9.789 seconds to a 10-second phase-budget timeout, before simulation.
That performance/diagnostic regression remains visible without a causal claim.
The full gate still fails coverage/runtime. See [fluid-review.md](fluid-review.md) and
[planar-perf.md](planar-perf.md) for source proofs, profiling, validation and
intermediate stage deltas. Fluid expansion remains paused.

Existing capability must be preserved by model identity, not net counts.
`fluid-preservation-targets.json` pins the union of 194 strict-high models from
current-main CI, the earlier `67978a5d` campaign cohort, and `456f1619`.
`fluid-high-water-targets.json` preserves that frozen roster plus NandGate and
DifferenceAmplifier, independently high in clean full `7ebb651bb`. The latest
full `7e67b9954` retains 192 of these 196 identities; four remain missing.
The [independent historical audit](preservation-roster-audit-2a0ea3aa.md) retains
source and artifact provenance, including limitations of older dirty reports.
`fluid-recovery-targets.json` retains the seven recovery obligations, including
the recovered PlanarFourbar, Engine1b and Fourbar1, so focused runs preserve scope.
`fluid-execution-recovery-targets.json` additionally pins all 13 raw execution
losses against the union of main and `67978a5d`, including those already
excluded from trace comparison. IMS_Start, the center-tap thyristor model,
Engine1b, Fourbar1 and PlanarFourbar now complete; eight remain unrecovered.
These completions must also be recovered;
an excluded model's execution recovery still does not establish parity.
These are focused verification rosters, not promoted baselines or substitutes
for the full 566-model gate. No gains offset a lost member; current compilation,
raw execution, and trace transitions must also remain visible in full diffs.

Current-main evidence is Nightly run
[35699258805](https://github.com/CogniPilot/rumoca/actions/runs/35699258805),
head `f477d0b698954b5a70f86286aaae9a3570ef39d5`. Its source tree exactly matches
the campaign's merged-PR starting commit `00ace1da`. Its full merged report has
191 strict-high / 191 compared, 21 tracked exclusions, zero missing, and 212
raw simulation completions. The merged quality/band artifacts say commit
`unknown`, while the trace artifact says `f477d0b69` with a dirty worktree;
the CI run binds the head, and these metadata limitations are retained.
Downloaded files and SHA-256 provenance live under
`target/fluid-campaign/main-f477d0b69-evidence/`.

The official `cargo xtask repo msl -- transition-diff` against `456f1619`
reports six main strict-high losses: IMC_YD, SMPM_VoltageSource, SMR_DOL,
UniversalConstraint, Fourbar1, and PlanarFourbar. CurrentControlledDCPM and
SpeedControlledDCPM enter strict-high, leaving a net decline of four;
TwoMass enters with deviation. Engine1b additionally remains a required
recovery from `67978a5d`. Evidence is `main-f477-vs-456-diff.{json,md}`.
The same comparison reports three compilation recoveries and zero compilation
losses, with eleven raw simulation completion losses against main.
The diff uses a lossless copy of the candidate's model/trace artifacts without
the optional package-rate JSON, which CI did not publish. Original artifacts
are unchanged. The first command's missing-package-artifact failure is retained.

TwoMass is now recorded as a reviewed initial zero-flow comparison boundary
and remains non-high; the review record retains the full source/trace proof.
The old canonical counts are unchanged. Preservation repairs and EmptyTanks
end-to-end validation remain required. All production repairs remain in compiler/shared Model Exchange;
no solver-specific model policy, tolerance relaxation, or baseline promotion
is authorized by this narrowed milestone.

Continuing adversarial review and outstanding objections are recorded in
[fluid-review.md](fluid-review.md). A direction approval does not accept a
candidate implementation or establish a compilation/parity gain.

- Integration branch: `fluid-library-coverage`.
- Starting commit: `00ace1da22e83edc0506df3549d80c8be18c7a11`.
- Library: pinned Modelica Standard Library 4.1.0.
- Reference compiler: repository-pinned OMC, reporting `a96aa1a-cmake`.
- OMC parsed-class inventory: `target/fluid-campaign/inventory/omc-class-inventory.json`.

Latest dedicated canonical Fluid run (historical frontier census): `target/fluid-campaign/after-record-index-rev2/`.
All 23 experiment models instantiate and pass typecheck; 20 now pass Flatten.
None completes DAE compilation or simulation, so Fluid trace parity is unmeasured.
The remaining failures are three canonical Flatten 10-second timeouts and
20 ToDae failures: ten parameter-evaluation errors, four missing constructor
identities, three vectorization-certificate refusals, two initial-algorithm
refusals, and one unresolved partial-function reference. All seven molarMass
failures are cleared. The fixed canary retains nine compared traces, all high, zero skipped/missing;
`canary-record-index-rev2-diff.{json,md}` records zero regressions or
compared-set changes.
The receiver-redeclaration review is closed: four source tests prove exact
callable instances and expected bodies; both receiver simulations and all
703 core tests pass at the preceding checkpoint.

The target is complete compilation of eligible concrete Fluid models and
strict-high OMC trace agreement for runnable Fluid models. Partial classes and
components needing an enclosing model must be identified explicitly in the
inventory. Compilation, simulation completion, and trace agreement are separate
results. Historical artifacts are triage inputs, not current verification.

An OMC scripting inventory (`getClassNames`, `getClassRestriction`, `isPartial`,
`isExperiment`, `getClassInformation`) finds 683 Fluid class declarations:
173 functions, 152 models, 143 packages, 96 records, 53 types, 53 general classes,
12 connectors, and 1 block. Of the 206 model/block/general-class declarations,
48 are partial and 158 are nonpartial. The 23 declarations with experiment
annotations exactly match the root-example target set. The other 135 nonpartial
declarations include components, nested helpers, and documentation; they are
tracked separately and are not silently counted as runnable examples. Their
direct-compilation diagnostic roster is `inventory/nonpartial-models.json`.
The durable roster is `infra/verification/fluid-compilation-targets.json`; it
also retains the 23 experiment names and the 48 explicitly partial declarations.
OMC's `getSourceFile(Modelica.Fluid)` confirms the pinned 4.1.0 source path.
Scripting API definitions are documented in the
[OMC scripting reference](https://openmodelica.org/doc/OpenModelicaUsersGuide/latest/scripting_api.html).

## Baseline run

```sh
CARGO_BUILD_JOBS=4 RUST_TEST_THREADS=4 RAYON_NUM_THREADS=4 \
  nix develop .#modelica --command cargo xtask verify msl-parity \
  --sim-match Modelica.Fluid. \
  --results-dir target/fluid-campaign/baseline \
  --stage-parallelism 4 --sim-parallelism 4 \
  --no-remote-quality-baseline
```

Result at the starting commit: the default root-example scope selects 23 Fluid
models; 0 reach DAE compilation and 0 simulations are attempted. The command
exits unsuccessfully because parity is unmeasured. Results are retained at
`target/fluid-campaign/baseline/`. This filtered run is diagnostic evidence;
it cannot establish a complete-cohort parity claim.

Repeating with `--all-omc-targets` and result directory
`target/fluid-campaign/baseline-oracle/` still skips reference generation when
no Rumoca simulation is attempted. That orchestration gap is assigned to the
validation worker; zero comparisons must continue to report unmeasured parity.

Reference generation uses the existing separate xtask utility:

```sh
CARGO_BUILD_JOBS=4 RUST_TEST_THREADS=4 RAYON_NUM_THREADS=4 \
  nix develop .#modelica --command cargo xtask repo msl omc-simulation-reference \
  --target-models-file target/fluid-campaign/baseline/msl_simulation_targets.json \
  --balance-results-file target/fluid-campaign/baseline/msl_results.json \
  --results-dir target/fluid-campaign/omc-reference \
  --workers 4 --use-experiment-stop-time
```

No baseline promotion is part of this campaign.

## Verification ledger

### Full cohort checkpoint 456f1619

The latest complete run is `target/fluid-campaign/tier2-shared-runtime-456f1619/`
at clean `456f161929594ae8d89d866e3c03cb75e2f038f2`. It records 204 raw
simulation completions, 188 compared models, 187 strict-high, one deviation,
16 tracked exclusions, and zero missing traces. Strict-high coverage is
187/566 (33.04%). The gate fails: TwoMass is newly executable but has 17
deviation channels (six severe) and 17 initialization deviations. This is an
actionable trace discrepancy until model-specific semantic evidence resolves
it. The current investigations must distinguish identified physical quantities
from any zero-flow algebraic ambiguity; family resemblance to another excluded
model is not sufficient evidence.

`tier2-zero-rhs-diff.{json,md}` records no departure from the previous compared
set: RevoluteConstraint enters strict-high and TwoMass enters deviation.
ParallelPumpDropOut also recovers execution under its existing exclusion.
IMS_Start loses raw execution to a 12-second timeout. Seven baseline-certified
strict-high models remain absent; `tier2-456-vs-baseline-diff` lists them.
Coverage and runtime ratchets still fail (runtime system median speedup
1.398672, floor 2.259809). Fluid expansion remains paused.

The preceding focused validation at this commit retains three compared focus
models and nine compared canary models, all strict-high with zero skipped or
missing traces and no transitions. ParallelPumpDropOut completes in
0.083692869 s but remains parity-unmeasured: zero compared, one tracked
exclusion, zero missing. A separate `cargo xtask ... plot-compare --reuse-traces`
audit of its retained canonical traces finds all five state temperature errors
below 0.0015 K, and the five worst junction channels differ only in the
zero-flow interval [0.4, 0.6]. This supports its existing boundary and does not
turn it into a strict-high model. The audit is
`parallelpump-zero-rhs-canonical/pointwise-window-audit.json`.

### Full cohort checkpoint ca785215

The latest complete 566-model run is
`target/fluid-campaign/tier2-shared-runtime-ca785215/`, from clean
`ca7852150e8071144ba2606416a9bf85b0b2b3b0`, using the canonical xtask command
and unchanged budgets/tolerances. It has 202 raw simulation completions,
186 compared models, all strict-high, 16 tracked exclusions, and zero missing
traces. Strict-high coverage is 186/566 (32.86%). No compared model has a
deviation channel. SpeedControlledDCPM remains 348 high/four minor/zero
deviation channels; OvervoltageProtection has all 45 channels high, and Vehicle
also recovers as strict-high. The original Speed counterexample stays closed.

The overall quality gate still fails coverage and runtime ratchets. Against
`tier2-observable-scaling-608ee8b9`, the official
`tier2-shared-runtime-diff.{json,md}` records two recoveries (Overvoltage and
Vehicle), one strict-high departure (RevoluteConstraint), and one additional
raw execution loss (ParallelPumpDropOut, previously a tracked exclusion).
RevoluteConstraint fails shared algebraic coordinate convergence;
ParallelPumpDropOut reports a singular algebraic sensitivity matrix. Both
regressions require shared-owner causal investigation; neither is waived.
Eight baseline-certified strict-high models remain absent. The strict-high
floor is 191 and classified floor 213; runtime system median speedup is
1.382924 against a 2.259809 floor. Fluid capability expansion remains paused.

The shared condition-history fix passes both source counterexamples with BDF
and RK45. Its canonical Tier 1 runs at the same commit retain three compared
focused models and nine compared canary models, all strict-high with zero
skipped/missing; `condition-focused-diff` and `condition-canary-diff` record no
transitions. Source tests, architecture checks, exact patches, and independent
review evidence are recorded in `fluid-review.md`.

### Full cohort checkpoint 67978a5d

The complete 566-model Tier 2 run at clean commit
`67978a5d263e726ddbfb596e4735f7b55d6a2e7c` is retained in
`target/fluid-campaign/tier2-67978a5d/`, with its command output in
`/tmp/rumoca-fluid-tier2-67978a5d.log`. It used `cargo xtask verify msl-parity`,
the fixed concurrency budgets, and pinned OMC `a96aa1a-cmake`.

Of 215 completed simulations, 193 traces were compared: 192 strict-high,
one near, zero deviation-band models; 22 were skipped under existing reviewed
exclusions and none lacked a trace. Strict-high coverage is 192/566 (33.92%).
The near result has four deviation channels, so the quality gate fails.
The remaining 351 targets comprise 278 not attempted and 73 simulation failures.
Fluid remains at zero completed simulations and has no measured trace parity.

The actionable counterexample is
`Modelica.Electrical.Machines.Examples.ControlledDCDrives.SpeedControlledDCPM`:
`currentController.addAntiWindup.u2`, `currentController.addSat.y`,
`armatureInverter.idealDcDc.powerController.u`, and
`armatureInverter.idealDcDc.feedback.y` disagree. Initial values agree;
trajectory errors require investigation. The gate separately reports a
baseline-certified regression for
`Modelica.Mechanics.MultiBody.Examples.Constraints.PrismaticConstraint`,
which exceeded the 10-second Solve budget at 11.896 seconds.

Compilation capability work is paused under SPEC_0033 §6a while the trace
counterexample is open. Separate author/reviewer pairs investigate the runtime,
trace comparison/publication, and timeout. No tolerance, exclusion, budget,
baseline, or retry changes close these failures.

`cargo xtask repo msl transition-diff` also compares the preserved historical
full run reporting `7ddbadf77` with this checkpoint. That older trace artifact
also reports `git_worktree_dirty=true`, so its complete source is not pinned.
Both contain all 566 targets;
191 compared models are common. CurrentControlledDCPM enters strict-high,
SpeedControlledDCPM enters near, and PrismaticConstraint and NandGate leave
the compared set. NandGate fails with BDF step-size exhaustion at `1e-13`.
This is a diagnostic historical comparison, not attribution to changes since our base
`00ace1da`. Evidence is `tier2-historical-7ddbadf-to-67978-diff.{json,md}`;
the original artifacts remain untouched in the user's workspace.

### Ordinary record-returning functions

Record binding expansion treated an ordinary function as a record constructor
and assigned positional arguments directly to record fields. In the Fluid
cases this assigned a pressure argument to the phase field. Constructor
projection now requires an exact resolved record class; a resolved function
cannot fall through to a same-named record. Ordinary function results retain
their field-access expression.

- Both focused regressions fail before the fix and pass afterward, including
  the legal source fixture. Confucius independently reviewed the exact delta.
- Main instantiate suite: 248 passed. The existing constructor fixture retains
  its assertions and now provides the canonical nested Pkg AST layout.
- Canonical run: `after-record-projection`, all 23 pass Typecheck. BatchPlant
  and PumpingSystem now fail Flatten at use_mu; InverseParameterization fails
  Flatten at molarMass. Full compilation and simulation remain 0/23.
- Fixed canary: `canary-after-record-projection`, nine compared traces all high,
  zero skipped/missing/deviation. The xtask transition report records no
  regressions or gains relative to `canary-after-alias-exposure`.
- Sources and exact digests: `record-projection-candidate-source/` under the
  campaign artifact directory.

### Callable alias exposure identity

An inherited alias was rewritten to its implementation DefId while retaining
the alias display spelling. Preserve the exposed declaration identity on that
call occurrence; the collected body retains the implementation identity.
The source regression fails with the old producer and passes with the repair.
Independent Leibniz review accepted the production delta and the additional
test assertions that distinguish exposure from implementation.

- Focused validation: 645 flatten unit tests and three source tests pass.
- Canonical Fluid run: `after-alias-exposure`, 0/23 compiled and simulated.
  HeatingSystem, RoomCO2, and RoomCO2WithControls move from EF019 to EF025
  for multiple `setState_pTX` exposures. Other failure families are unchanged.
- Fixed canary: `canary-after-alias-exposure`, 11 compiled, nine completed
  simulations, nine compared traces all high, zero skipped/missing/deviation.
  `cargo xtask repo msl transition-diff` records no stage, simulation, or band
  regressions against `canary-after-member-cache` in
  `canary-alias-exposure-diff.{json,md}`. This is Tier 1 evidence only.
- Exact candidate file digests and original sources are retained in
  `target/fluid-campaign/alias-exposure-candidate-source/`.

### Held enclosing-record candidate

The indexed candidate passed its seven source tests but failed one of 248
instantiate tests: a legal record redeclaration inside an extends modifier.
The guard checked `is_redeclare` on the selected ordinary RHS record rather
than proving the modifier's slot. The ten candidate files were restored and
review reopened; no canonical gain is claimed for that candidate.

### Inherited record member selection

The initial DrumBoiler failure is `EI007` for
`Medium.fluidConstants[1].criticalPressure`. A source regression reproduces it
with the same multilevel package inheritance and short record redeclaration.
Resolve correctly retains the virtual `PartialMedium.FluidConstants` slot;
instantiation incorrectly follows that slot's default record when proving the
next member. Rebase the intermediate record type through the selected owner's
declaration-ID map before proving the next member. Also traverse every inherited
package level when collecting those mappings. The regression checks the selected
member DefId and rejects borrowing members from an unselected record.

Governing contracts: SPEC_0001 identity domains; SPEC_0007 stage ownership;
SPEC_0022 INST-002, INST-014, INST-044 (MLS §7.3); SPEC_0033 §2–§3.

- Before focused test: `EI007`, missing `criticalPressure`.
- After focused test and full `cargo test -p rumoca-phase-instantiate`:
  231 passed, zero failures; one ignored documentation example.
- Fluid xtask result: `target/fluid-campaign/after-record-members/`.
  DrumBoiler now reaches Flatten and fails `EF025` for `Medium.dewEnthalpy`.
  Total compilation remains 0/23; the failure census is now 13 `ET000`,
  8 `EI012`, 1 `EI027`, and 1 `EF025`. Trace parity remains unmeasured.
- Fixed 20-model canary: before and after both compile 11 models, complete
  9 simulations, and compare 9 traces with no deviations. These are Tier 1
  results only, not complete-cohort parity numbers.
- Canary directories: `target/fluid-campaign/canary-before/` and
  `target/fluid-campaign/canary-after-record-members/`. The initial before
  report stage lacked Node; completing `cargo xtask repo msl
  omc-simulation-reference` with Node reused its traces without repeating
  Rumoca compilation or simulation attempts.

### OMC diagnostic evidence

The separate default 12-second reference attempt completed 8/23 models and
timed out on 15. Its JSON was written, but HTML reporting initially lacked
Node. A separate diagnostic run with `--model-timeout-seconds 120`, retained
at `target/fluid-campaign/omc-diagnostic-120s/`, completed 21/23, reported one
initialization assertion failure in HeatingSystem, and timed out on one model.
These longer diagnostic budgets do not change canonical gate budgets or
establish any Rumoca trace agreement.

Complete-cohort evidence remains pending.

### Explicit OMC reference generation with zero Rumoca attempts

Both the stats-report dispatcher and reference-stage guard skipped OMC when
`sim_attempted == 0`. They now share the explicit diagnostic-lane predicate,
which honors `--all-omc-targets`. A callback-order regression covers the outer
dispatcher; zero-comparison measurements still fail closed.

The canonical integration command selects MeasuringTemperature with
`--sim-match-exact --all-omc-targets` and writes
`target/fluid-campaign/harness-zero-comparison-fixed/`. Rumoca attempts no
simulation, OMC successfully generates one reference, and the outer xtask
command exits unsuccessfully because `models_compared == 0`. This is the
expected result: reference availability is distinct from measured parity.

The full quality-gate unit filter passes 98 tests. The no-simulation test now
passes its diagnostic option explicitly, so a prior integration run's persisted
configuration cannot make the unit test invoke OMC. Two stale assertions were
updated to match the already-committed baseline fixture (493 Flatten successes
and 192 certified models at its recorded commit); the baseline file and all
runtime ratchets remain unchanged.

All four stats-report tests also pass with `--profile msl-fast --features
msl-full-test`, including both comparator-before-gate ordering regressions.
An initial invocation without `msl-full-test` discovered zero tests and is not
counted as validation.

### Dotted medium selection and inherited aliases

`Name.def_id` already preserves the lexical package identity. Instantiation
incorrectly bypassed dotted type selection when a stale resolved member ID was
present, and its nested class-override payload retained only one of the inherited
declarations for an effective alias. Honor the selected package despite the stale
default member and carry every inherited alias DefId into the instance payload.
Explicit instance overrides retain precedence after inherited specialization.

The combined instantiation library suite passes 232 tests. The Fluid sweep at
`target/fluid-campaign/after-medium-selection/` still compiles 0/23, with
11 `EI012`, 10 `ET000`, one `EI007`, and one `EI027`. Four models now stop in
instantiation instead of typecheck, and DrumBoiler now stops in typecheck
instead of flatten. These changes remain under investigation; the original
focused selection regression passing does not establish that the Medium
selection failure family is closed. BatchPlant's new `EI007` concerns a
dynamically selected intermediate component without a resolved type identity.

The fixed canary at `target/fluid-campaign/canary-after-medium-selection/`
retains 11 compilations, 9 successful simulations, and 9 high trace comparisons
with zero deviations. Its two attempted solver failures are unchanged.

### Outer-to-inner semantic identity

Typecheck now bridges an outer component's resolved instance identity to its
inner target before walking member declaration identities. Fully resolved
misses still abstain rather than retrying a rendered name. Regressions cover
wrong root/owner/tail identities, absent targets, and ambiguous targets. The
typecheck library suite passes 127 tests. Integration evidence at
`target/fluid-campaign/after-outer-identity/` still compiles 0/23: 11 `EI012`,
8 `ET000`, 2 `ET002`, one `EI007`, and one `EI027`. The remaining scalar type
errors incorrectly identify component types as their enclosing Medium package;
their producer is under investigation. The fixed canary at
`target/fluid-campaign/canary-after-outer-identity/` retains 11 compilations,
9 successful simulations, and 9 high trace comparisons with zero deviations.

### Dynamic intermediate member selection

BatchPlant's `medium` component legitimately has no static `type_def_id` because
its `Medium.BaseProperties` type depends on the instance's selected package.
The member walker now carries the active instance override map and resolves
that dotted type through its existing package declaration identity before
walking the tail. Existing record-alias selection keeps its direct slot mapping.
The combined instantiation suite passes 233 tests; strengthened focused tests
assert the exact final member `DefId` and reject borrowing a missing member from
an unselected default package.

The canonical Fluid sweep at `target/fluid-campaign/after-dynamic-members/`
moves BatchPlant from `EI007` in Instantiate to the existing Medium type errors
in Typecheck. Other model diagnostics are unchanged. Compilation remains 0/23,
with 11 `EI012`, one `EI027`, 9 `ET000`, and 2 `ET002`. The fixed canary at
`target/fluid-campaign/canary-after-dynamic-members/` retains 11 compilations,
9 successful simulations, and 9 high trace comparisons with zero deviations,
zero skipped comparisons, and zero missing traces. Four strengthened focused
member-selection tests pass after integration. Canonical transition diffs for
each successive canary are retained beside the run directories.

### Redeclaration constraint context

HeatExchanger's short replaceable HeatTransfer class alias was checked against
the bare constraining class, losing the replaceable slot's interface context.
Redeclaration entry points now pass that context into a fresh subtype cache.
Transitive non-replaceability follows the existing declaration/reference proof
instead of inspecting whether a class contains replaceable members.

Review also found that exact class-kind equality rejected Modelica's permitted
model/block and type/record combinations. MLS §6.4's compatibility groups now
govern that check; function, package, and connector boundaries remain distinct.
Pinned OMC accepts the short-alias, long-definition, nested-member, and
model/block fixtures. It rejects the used function-for-model negative under
both `checkModel` and `instantiateModel`.

The combined instantiation suite passes 239 tests. Canonical integration
evidence at `target/fluid-campaign/after-subtype-context/` moves HeatExchanger
from `EI027` to `EI012` for `ambient2.medium`. BatchPlant instead exceeds its
10-second Instantiate budget at 11.468 seconds; that timeout remains a failure
and its cause is under investigation. The sweep still compiles 0/23, with
12 `EI012`, 8 `ET000`, 2 `ET002`, and one timeout. This result does not close
the Medium selection family.

The fixed canary at `target/fluid-campaign/canary-after-subtype-context/`
retains 11 compilations, 9 successful simulations, and 9 high trace comparisons
with zero deviations, skipped comparisons, or missing traces.

A subsequent bounded optimization skips building the complete class index
when the slot is already known to be replaceable and all other class-level
checks have passed. Bare/non-replaceable queries retain the ancestry proofs.
The 35 focused inheritance tests pass. The fresh Fluid attempt at
`target/fluid-campaign/after-subtype-fastpath/` still times out BatchPlant in
Instantiate at 12.113 seconds and otherwise retains the same diagnostics.
This optimization has not closed the timeout; profiling remains pending.

An isolated diagnostic at `target/fluid-campaign/diagnostic-class-index/`
measured five class-index constructions totaling approximately 11 ms, but its
compiler lacked other integrated prerequisites and stopped earlier at
`volume5.medium`. It does not explain the timeout in the full candidate;
comparable profiling must use that candidate's complete source snapshot.

### Forwarded package identity and short aliases

Resolve correctly assigns the enclosing package alias identity to the RHS of
`redeclare package Medium = Medium`. The nested-scope consumer instead queried
the nested target slot. It now first follows the resolved RHS reference through
the active override map. The removed alias-name fallback remains absent.

Unmodified short replaceable class definitions now select their resolved
default target rather than themselves. The source `end_name_token`
distinguishes long definitions, which retain their own identity even when they
contain only an extends clause. Modified aliases retain their class context.

The combined instantiation suite passes 241 tests, including a source-based
self-forwarding record-member regression and the short/long distinction.
Canonical evidence at `target/fluid-campaign/after-forwarding-aliases/` clears
Instantiate for all 23 Fluid experiment models. BatchPlant takes 9.736 seconds
in Instantiate in this new-code attempt; the earlier timeouts remain recorded.
The sweep still compiles 0/23: 21 models stop at `ET000`, and DrumBoiler plus
MomentumBalanceFittings stop at `EF025` for selected Medium functions. The
incorrect whole-package scalar type errors also disappear after this producer
repair. Remaining typecheck families concern selected ThermodynamicState
records, outer parameter variability, and inherited connector members.

Differently named forwarding is a separate pending change and must revalidate
the selected target against the nested slot's constraint.

The fixed canary at `target/fluid-campaign/canary-after-forwarding-aliases/`
retains 11 compilations, 9 successful simulations, and 9 high trace comparisons
with zero deviations, skipped comparisons, or missing traces. Its canonical
transition diff records no lost comparisons or band changes.

### Residual outer-reference identity proof

A two-base source reproduction shows that inherited `outer Sys system`
declarations are merged while one binding retains the discarded declaration's
root `DefId`. The retained component uses `DefId(104)`; the bindings use roots
104 and 106 with the same tail member 103. Correcting only that stale root in
the reproduction removes the false variability diagnostic. The pending fix
belongs to inherited-declaration identity production, scoped to the merged
instance/class; typecheck's strict missing-identity behavior remains intact.

### Differently named forwarding

Component class-override extraction now receives the active instance override
map and selects the normalized RHS reference before checking constraints or
resolving modifier arguments. This handles `redeclare package Medium = M`
without a second validation/repair pass or a rendered-name fallback. The
stored target reference retains its resolved identity. The negative regression
rejects an actual package that satisfies the enclosing broad constraint but
violates the nested slot's narrower constraint.

All 243 combined instantiation tests pass, including seven selected-member
and forwarding regressions. Canonical evidence at
`target/fluid-campaign/after-forwarded-actuals/` retains the preceding model
diagnostics except that BatchPlant again times out in Instantiate at 11.592
seconds. The latest run therefore clears Instantiate for 22/23 and compiles
0/23, with 20 `ET000`, 2 `EF025`, and one timeout. The previous all-23
instantiation result does not erase this reliability failure.

The fixed canary at `target/fluid-campaign/canary-after-forwarded-actuals/`
retains 11 compilations, 9 successful simulations, and 9 high trace comparisons
with zero deviations, skipped comparisons, or missing traces. Its canonical
transition diff records no lost comparisons or band changes.

### Exact package slots for function selection

Flattening's package override collection previously collapsed inherited
declaration slots by their displayed alias. It now retains exact package slot
entries separately, carries available slot `DefId`s from every package
producer, and prefers exact slot selection before a lazy source-package query.
Distinct inherited slots survive scope collection. Missing extends identities
and ambiguous inherited candidates remain unresolved; constructor/redeclare
producers no longer recover those identities from the base name.

The isolated candidate passes 642 flatten unit tests and three parsed-source
identity integration tests. Direct producer negatives cover absent base IDs
despite a valid displayed name. Independent review verified exact source-slot
selection, ambiguity handling, and constructor/default identity propagation.
Combined main validation also passes all 642 unit tests and three source
identity integration tests. The canonical Fluid sweep at
`target/fluid-campaign/after-function-slots/` clears both `EF025` errors:
DrumBoiler and MomentumBalanceFittings now report `EF024`, missing structured
identity for `T`. All 23 models complete Instantiate in this run (BatchPlant:
9.312 seconds), but compilation remains 0/23, with 21 `ET000` and 2 `EF024`.
Earlier instantiation timeouts remain part of the reliability investigation.

The fixed canary at `target/fluid-campaign/canary-after-function-slots/`
retains 11 compilations, 9 successful simulations, and 9 high trace comparisons
with zero deviations, skipped comparisons, or missing traces. Two attempted
solver failures remain visible. The canonical transition diff is retained
beside these run directories.

### Inherited connector member types

The typecheck member catalog omitted inherited `Medium.*` types whose final
type declaration was deferred. It now walks the anchored name's structured
segments through scope-tree member lookup and maps the final declaration ID
to its type. Missing anchors, missing members, and ambiguous inherited members
produce no identity. A qualified display name cannot substitute for the
missing anchor in this deferred branch.

The combined typecheck library passes 132 tests. In an isolated pinned-MSL
TwoTanks diagnostic, the targeted FluidPort/VesselFluidPorts_a unknown-member
errors disappear; selected `medium.state.p/T` errors remain separate. Pinned
OMC accepts TwoTanks under both checkModel and instantiateModel. Canonical
main evidence at `target/fluid-campaign/after-connector-members/` removes the
targeted FluidPort/VesselFluidPorts unknown-member diagnostics from all 14
models that reported them in the preceding sweep. All 23 models instantiate,
but compilation remains 0/23 because 21 retain other `ET000` diagnostics and
two retain the `EF024` function-variable identity error.

The fixed canary at `target/fluid-campaign/canary-after-connector-members/`
retains 11 compilations, 9 successful simulations, and 9 high trace comparisons,
with zero deviations, skipped comparisons, or missing traces. The canonical
transition diff reports zero simulation regressions, high-band regressions,
band changes, or lost comparisons. These are focused Tier 1 results only.
An additional producer regression clears the anchor while retaining a valid
fully qualified type-table entry and verifies that no textual lookup restores
the missing identity.

### Selected record member candidate (rejected)

Reconstructing a selected package's inherited record map during deferred member
proof passes 248 combined instantiation tests, including distinct forwarded
and sibling media. The actual Fluid comparison at
`target/fluid-campaign/after-record-state/` does not close the MSL defect:
all 23 instantiate, but 21 still fail typecheck and two fail flattening; every
typecheck-failing model still reports ThermodynamicState diagnostics. The
candidate is not evidence that the selected-state producer defect is fixed.
Its fixed canary compares nine models with nine high bands and zero skipped
or missing traces; the xtask transition diff records zero regressions or band
changes. At that point, scope safety and the actual MSL producer had not been
established.

Subsequent source-level review proves a scope collision: one instance with
simultaneous `A = Water` and `B = Oil` package selections resolves both
`a.state.T` and `b.state.T` to Oil's field. Pinned OMC accepts and instantiates
the fixture; the candidate produces Oil field DefId 109 for both references
instead of Water 108 and Oil 109. The earlier sibling-instance fixture passes
and therefore did not cover this same-instance collision. The broad active-
package specialization and its five candidate tests were removed from the
integration tree; the candidate and failing fixture remain isolated for repair.
It is not an accepted capability or an MSL improvement.

After removal, the combined instantiation/typecheck suites pass 246/136 tests.
`target/fluid-campaign/after-record-candidate-removal/` retains 20 typecheck
failures, two flattening failures, one instantiation timeout, and zero variability
diagnostics among the models reaching typecheck. The fixed canary compares
nine traces, all high, with zero skipped/missing traces; its xtask transition
diff records no regressions or band changes.

The retained integration candidate also passes combined AST/instantiate/
typecheck/flatten Clippy with all targets and warnings denied, workspace
rustfmt, and `git diff --check`.

Combined instantiate/typecheck/flatten Clippy (`--all-targets -- -D warnings`)
and workspace rustfmt checks pass at this candidate. The connector producer
negative is included in the now-133-test typecheck suite. The remaining
typecheck census contains 131 variability diagnostics across nine models and
534 unknown-member diagnostics across the 21 failing models; these diagnostic
totals describe the current frontier, not additional model failures.

### Duplicate inherited outer declaration identity (candidate)

The inheritance merge retains one of two identical `outer system`
declarations but previously preserved references to the discarded declaration.
Record the exact source-to-retained declaration map in the merging class and
attach it to each concrete class occurrence. Copied component expressions
canonicalize only their root segment; nested member identities remain owned by
their selected component instances. Typecheck uses the same occurrence-scoped
map when examining immutable inherited declarations. Missing identities remain
strictly missing, and cyclic maps cannot cause an infinite loop.

The combined candidate passes 71 AST tests, 251 instantiation tests, and 135
typecheck tests. The canonical Fluid run at
`target/fluid-campaign/after-duplicate-outer/` removes all 111 variability
diagnostics across the eight affected models that reach typecheck. BatchPlant
times out during instantiation at 11.443 seconds against the 10-second budget;
its 20 previous variability diagnostics cannot be assessed in this run.
Compilation remains 0/23, with 20 `ET000`, two `EF024`, and that timeout.
Its fixed canary at `target/fluid-campaign/canary-after-duplicate-outer/`
compares nine models with nine high bands and zero skipped or missing traces;
the xtask transition diff records zero regressions, band changes, or lost
comparisons.

A follow-up composes cached branch maps through the declaration retained by the
current merge. The valid opposite-order diamond now preserves `B -> A` instead
of dropping both identities. A parsed-source regression verifies both inherited
parameter bindings and actual typecheck against a root inner declaration.
Combined instantiation and typecheck suites pass 251 and 136 tests respectively;
the correction's canonical Fluid run at
`target/fluid-campaign/after-duplicate-outer-diamond/` retains zero variability
diagnostics in the 20 models reaching typecheck, two `EF024` failures, and one
BatchPlant instantiation timeout. Its fixed canary compares nine models with
nine high bands, zero skipped/missing traces, and zero transition regressions.
The same-timestamp-rounded BatchPlant timeout was checked against the raw
durations: these are separate attempts (11.442689753 and 11.443190009 seconds).

### Remaining modifier binding context defect (triage)

Actual OneTank tracing distinguishes the inherited `state` component declaration
from its concrete record type; those identities are not interchangeable.
Instantiation records the modifier's lexical `binding_source_scope` correctly.
`check_instanced_bindings` uses that lexical path while setting
`current_class_instance_id` to the modified component's owner. Fully resolved
reference lookup therefore searches under the wrong class occurrence. A probe
that omitted BindingSource member proof selected a different lookup path and
removed the thermodynamic diagnostics, but it bypassed the proof obligation and
was rejected and removed. The valid repair must pair the source expression,
lexical path, and lexical class occurrence without relaxing identity lookup.

The integrated repair resolves the recorded structured source path to exactly
one class `InstanceId` and pairs that identity with the source expression.
Missing, unresolved, and ambiguous source scopes emit typed errors. Synthetic
legacy fixtures were given the class metadata that production instantiation
already supplies; fully resolved fixture strengthening remains in progress.
The typecheck library passes 139 tests.

The canonical run at `target/fluid-campaign/after-binding-source-scope/`
reduces typecheck failures from 20 to two. Nineteen models reach Flatten and
ControlledTanks reaches ToDae. Compilation remains 0/23: 16 `EF024`, three
`EF019`, two `ET002`, one `ED019`, and one BatchPlant instantiation timeout
(13.453 seconds). New failures remain visible: `use_mu`, `molarMass`, another
`T` field, inconsistent structured function references, FixedPhase/pressure
argument types, and ControlledTanks' enumeration-times-real parameter binding.
The fixed canary compares nine traces, all high, with zero skipped or missing
traces. These remain Tier 1 observations, not Fluid or cohort parity claims.

### Callable record member identity

Callable conversion lowers source function bodies without instantiating them as
component occurrences. Deferred record fields therefore require proof against
the selected callable's exact lexical record redeclaration. The new selection
follows compiler-owned `redeclare_target_def_id` chains, requires exact formal
root identity, accepts repeated inheritance paths only for the same member
identity, and leaves unrelated/ambiguous candidates unresolved. It removes the
previous rendered type-name resolution from this selection path.

Combined main validation passes 642 flatten unit tests and two source suites
of three tests each. At `target/fluid-campaign/after-function-record-identity/`,
DrumBoiler and MomentumBalanceFittings both progress from `EF024` at `T` to
`EF024` at `molarMass`. Compilation remains 0/23, with the other 20 models
failing typecheck and BatchPlant timing out during instantiation. The final
root-identity, diamond, and unrelated-derived-record regressions pass: 643
flatten unit tests and five record-identity source tests. No Fluid trace
comparison is possible yet.

The fixed canary at `target/fluid-campaign/canary-after-function-record-identity/`
compares nine traces, all high, with zero skipped/missing traces and zero
transition regressions. An initial transition-diff invocation was premature
while the canary waited for a configuration lock; it produced no comparison.
The final diff was generated after the run completed. The blocking diagnostic
workspace's shared configuration symlink was replaced with an independent MSL
directory; only the pinned library directory remains shared. The stopped
diagnostic launcher had not yet begun a model attempt.

### Effective component lookup cache

BatchPlant profiling identifies repeated effective-component construction as
the main measured member-resolution cost: 1,940 constructions consume 7.153
seconds in the instrumented diagnostic. ClassDefIndex construction is not the
dominant cost. The cache retains structural effective components by exact owner
DefId within an immutable-tree resolution scope; active instance overrides are
applied afterward. Current callers cannot pass modified class clones into this
cached lookup and do not share caches across trees.

An identical isolated BatchPlant helper run retains its ET000 diagnostic hash
and exit code while decreasing from 11.046 to 9.880 seconds (10.6%). That is
diagnostic timing, not a canonical budget pass. The integrated patch passes
246 instantiation tests and 139 typecheck tests. Canonical verification at
`target/fluid-campaign/after-member-cache/` completes BatchPlant instantiation
in 6.911 seconds, within the unchanged 10-second budget. It now exposes the
same FixedPhase-versus-AbsolutePressure type error as InverseParameterization
and PumpingSystem. All 23 targets instantiate, and 20 pass typecheck.
The previous 20 BatchPlant variability diagnostics are now verified absent.
Compilation remains 0/23: 16 `EF024`, three `EF019`, three `ET002`, and one
`ED019`. Earlier timeout attempts remain in the evidence history.

The fixed canary at `target/fluid-campaign/canary-after-member-cache/`
retains nine compared traces, all high, with zero skipped or missing traces.
`cargo xtask repo msl transition-diff` records zero simulation or high-band
regressions and no change in the compared set. The source-scope binding loop
was subsequently split into a per-binding helper to satisfy the existing
clippy nesting limit without changing its lookup or rejection behavior.

### Integration architecture checks

An experimental two-file function-signature identity change passed 140
typecheck tests but did not improve the real models. The canonical command at
`target/fluid-campaign/signature-candidate/` retains exactly three ET002,
16 EF024, three EF019, and one ED019, with 0/23 compiled or simulated. The
candidate was reverted; no canary gain or capability closure is claimed.

The initial architecture suite passed 241 tests and found two issues. The
diagnostic registry scan counted an external test module's assertion about
`ER002` as typecheck production ownership. Registry discovery now uses the
existing compiler-reachability-based production source walker. A fixture
checks that a production module named `tests.rs` remains included while a
test-gated module referencing another phase's code is excluded. The 13
diagnostic contract tests pass without changing any diagnostic ownership.

The all-OMC reference-stage predicate pushed the quality-gate parent file over
the 2,000-line limit. It now lives in the existing reference-stage module.
The complete architecture and repository policy suites pass 244 and 17 tests,
respectively. The source-scope per-binding refactor also retains all 139
typecheck tests and passes clippy for instantiate, typecheck, and flatten.
The relocated reference-stage helper retains all 98 quality-gate tests.
Subsequent scope-fixture review found that the first strengthened tests still
passed with the defect restored. A replacement two-medium occurrence fixture
fails with the old pairing and passes with the correct one; its cross-medium
negative remains rejected. The current typecheck suite passes 142 tests and
clippy. See the review record for the exact challenge and pending peer review.

### Broader declaration diagnostic

`cargo xtask verify msl-parity --sim-targets-file
target/fluid-campaign/inventory/nonpartial-models.json` at the record-member
candidate processes all 158 nonpartial declarations. Results at
`target/fluid-campaign/all-declarations-before/` compile 59; the 99 failures are
53 `ET000`, 22 `EI012`, 8 `ET002`, 5 `EF024`, 4 `EF025`, 3 `ED010`, 2 `EI027`,
1 `ET001`, and 1 `EF019`. This roster includes library components without
enclosing configurations and documentation classes. It is a diagnostic census,
not a roster of legal standalone simulations. Its zero comparable traces leave
parity unmeasured despite completion of empty/trivial models.

### Inherited package defaults

The upstream collection fix clears all 14 `use_mu` failures. The current
23-model canonical run retains all 23 Instantiate and Typecheck passes, with
12 EF025, seven EF024 molarMass, three canonical Flatten timeouts, and one
ED019 ControlledTanks failure. Compilation and simulation remain zero; Fluid
parity is unmeasured. The focused instantiate suite passes 249 tests, with
old-red/new-green evidence and independent Locke review.

The fixed canary at `target/fluid-campaign/canary-after-inherited-alias-defaults/`
retains nine compared traces, all high, zero skipped/missing, and no deviation.
`canary-inherited-alias-defaults-diff.{json,md}` records zero simulation or
high-band regressions and no compared-set changes. This is a canary delta,
not a complete-cohort result.

### Broader checkpoint validation

The existing receiver simulation regressions pass both direct/outer calls and
distinct sibling modifier values (two tests). The initial architecture rerun
found `functions/tests.rs` at 2,080 lines. Moving the two new exposure tests
verbatim into the existing lexical-exposure module restores all 244
architecture checks and 17 repository policy checks; the five tests in that
module pass. No production behavior changed in that move.

The accepted record-index checkpoint also passes the complete 703-test core
suite, all 244 architecture checks, and all 17 policy checks. The rejected
typed-owner candidate is absent. Full-cohort verification remains outstanding;
no complete-cohort or Fluid parity number is claimed.
