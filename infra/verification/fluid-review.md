# Fluid campaign review record

This is the working review record for the Fluid campaign, implementing the
requested continuing adversarial review and SPEC_0033 sections 2–6a. The
governing compiler contracts remain in the specs. Current measurements and
commands live in [fluid-campaign.md](fluid-campaign.md).

## Review before integration

To keep review economical, only the two active implementation workstreams run
continuously. Independent reviewers activate for a concrete evidence bundle or
changed patch. Handoffs contain a short result, artifact paths, and the next
action; unchanged evidence is not repeatedly reviewed. This preserves every
acceptance check below while avoiding overlapping investigations and repeated
status-only passes.

For each candidate, retain:

1. The exact source snapshot and proposed diff, with a digest when ready for
   review. Review applies to that diff; subsequent changes reopen review.
2. The real failing model, expected behavior, and first incorrect producer
   output. Reporting a later validator failure alone does not establish cause.
3. The strongest competing explanation and the evidence that rejects it.
4. A focused regression that fails without the fix and passes with it, plus
   a meaningful negative or isolation case. Passing an existing test alone is
   insufficient evidence for a new capability.
5. An independent challenge from a different workstream, followed by the
   integration decision. The implementation author cannot close their own
   review. Unresolved correctness objections hold the candidate.
6. Focused validation and the fixed canary transition after integration.
   Broader parity claims require the named-commit full cohort run prescribed
   by SPEC_0033; compiler or simulation counts cannot substitute for traces.

Review challenges include wrong declaration or occurrence identity, two
different redeclarations in one model, unresolved or ambiguous inputs,
defaulted or inherited function arguments, and new repeated whole-tree work.
Choose the challenge that could falsify the proposed semantics. Validators,
tolerances, target exclusions, retries, and name-derived identities cannot
turn missing proof into a pass.

Every handoff carries its review status, pending objections, evidence paths,
and next smallest check. Context resets do not close outstanding reviews.
Central integration owns the measured runs; independent reviewers normally
read the diff and evidence before requesting additional builds.

At every handoff, unexpected result, and resumed context, the orchestrator
checks this record against the current source and measurement artifacts before
continuing. An old approval cannot cover a revised diff. Code-review approval
may admit a candidate to the validation runner; acceptance still requires the
recorded checks to pass. Reviewers must support objections with a concrete
contract violation or counterexample and explicitly retract disproven claims.

## Current direction review

### Bulk Jacobian clears recover Engine1b and Fourbar1 at 7e67b9954

Clean `7e67b9954`, full run `tier2-bulk-clear-7e67b9954/`, covers all 566
targets: **211 raw completions, 192 compared, all 192 high, 19 tracked
exclusions, zero missing**. The official `bulk-clear-tier2-diff` preserves all
190 prior high identities and all 208 prior raw completions. Engine1b and
Fourbar1 recover high parity in 7.396 and 8.609 seconds. PlanarFourbar remains
high in 5.538 seconds; IMS_Start retains raw completion in 9.796 seconds.
ThyristorCenterTap2Pulse_RLV_Characteristic recovers raw execution in 11.791
seconds, closing its loss against `2a0ea3aa`; its existing comparator exclusion
remains, so this is not a high-parity gain.

Historical preservation is **192/196**, or 190/194 on the original roster.
IMC_YD, SMPM_VoltageSource, SMR_DOL and UniversalConstraint still time out.
Five of the unchanged 13 raw recovery obligations complete: IMS_Start,
the center-tap thyristor model, Engine1b, Fourbar1 and PlanarFourbar. No roster,
budget, tolerance, solver policy or comparator boundary changed. The full gate
still fails coverage/accounting, runtime ratios and the four certified-model
losses. The strict-high count now exceeds its 191 floor; that does not excuse
the remaining identity losses. `bulk-clear-preservation-audit.json` retains
the exact sets.

The intermediate Electrical.Machines Solve count decreases by one:
DC_CompareCharacteristics previously reached an EL005 structural error after
9.789 seconds, and now exceeds the unchanged 10-second Solve phase budget.
Neither run reaches simulation. Whole-model compilation classifications are
unchanged. This performance/diagnostic regression remains visible; no retry
or source-causality claim is made.

Worker `285d6c15e` integrates as `7e67b9954`. Exactly two dense-owned Jacobian
clears use `as_mut_slice().fill(0.0)` instead of nalgebra's generic iterator.
Both complete clears remain, including the clear after prepared evaluation
declines. Validation, fresh derivatives, native error routing/publication,
fallbacks and residual/correction certificates remain unchanged.

The single actual-canonical-setting SMPM profile at `af33fc5b8` records an
11.015-second interior window with 10.89 seconds user CPU and 0.04 seconds
system CPU. Raw RBP chains and frozen ELF disassembly attribute 183/2190 CPU
samples (8.36%) to the first clear's scalar zero-store loop. Both candidate
clears lower to zero-valued `memset` over the complete contiguous storage.
Noether independently reviewed the attribution and frozen two-line patch.
This profile is diagnostic and does not establish SMPM recovery; SMPM still
times out in both focused and full candidate runs.

Worker checks pass: 4 storage controls, 10 prepared-Jacobian controls, all 559
solver tests, strict solver clippy and formatting. Main passes 2 prepared
source tests, 7 coupled-refresh source tests, 3 fixed-boundary source tests,
simulation clippy and workspace formatting. Fixed canary: 9 compared/high,
zero skipped/missing, no transitions. Unchanged eight-model recovery set:
4 compared/high, zero skipped/missing, four timeouts; the strict selected-target
gate correctly fails after comparison. Evidence lives in
`target/fluid-campaign/bulk-clear-review/frozen-285d6c15e/`, the integrated
checks log, and the canary/recovery/full transition artifacts. The raw profile
is retained at `/tmp/rumoca-fluid-speed-bdf/target/smpm-perf-canonical-af33/`.

Noether's independent full audit preserves every prior high model's channel
counts and distributions. Engine1b has 1293/1293 high channels and Fourbar1
854/854. The audit is retained at
`target/fluid-campaign/bulk-clear-review/independent-full-audit-7e67b9954.md`.

Follow-up attribution rejects two weak directions: layout comparison accounts
for only 3/2190 samples, and repeated per-entry scale sanitization for only
19/2190. The latter would require additional admission scans/branches; no
implementation is authorized on this evidence. All numerical scales remain
fresh and no new cache is introduced. Fluid expansion remains paused.

### Full cohort closes the IMS regression at af33fc5b8

Clean `af33fc5b8`, run `tier2-dense-lu-owned-af33fc5b8/`, covers all 566 targets:
**208 raw completions, 190 compared, all 190 high, 18 tracked exclusions,
zero missing**. Official `dense-lu-owned-tier2-diff` preserves all 190 prior high
identities and records IMS_Start raw recovery at 9.724 seconds, with no raw
completion loss, whole-model compilation loss, or band change. IMS retains its
existing comparator exclusion and is not a high-parity gain. PlanarFourbar
remains high, completing in 9.432 seconds. Historical preservation remains
190/196 (188/194 original), with the same six timeouts. Two of the fixed 13 raw
recovery targets now complete: IMS_Start and PlanarFourbar. The thyristor model's
raw loss against `2a0ea3aa` remains open. Coverage/runtime gates still fail.

Worker `0700a587f` integrates as `af33fc5b8`: the shared dense Newton path
consumes its locally owned scaled matrix in LU. Only an allowed direct failure
or nonsquare case rebuilds identical scaled values from immutable inputs for the
existing SVD fallback. Sparse policy, square guard, RHS, tolerance, and nonfinite
`Some` behavior remain unchanged. No numerical cache, new solver algorithm, or
allocator setting changes. Noether approved the frozen source independently.

The old source fails a control comparing the actual scaled-input and LU-factor
storage addresses; the candidate consumes that allocation. Six focused controls
check old-policy bitwise equivalence and input preservation across pivoting,
singular/rectangular cases, disabled fallback, nonfinite results, and sparse
success/decline. Main passes 559 shared tests, 12 source tests, simulation clippy,
and formatting. Fixed canary: nine compared/high, zero skipped/missing, zero
transitions. Unchanged eight-model recovery list: two compared/high, zero
skipped/missing, six timeouts, zero transitions; its strict gate still fails.

One sequential ordinary-build IMS diagnostic pair under matching canonical
worker settings measures `7fed0a809` 14.662 seconds versus `af33fc5b8` 9.580.
The full 802-channel, 15,003-point traces over 0–1.5 seconds are byte-identical,
SHA-256 `3bc27a496ae17fbb7018f70365c001b9b065421d20b843b9936dfe1b70e1fae6`.
Interior system CPU falls 3.09 to 0.06 seconds, minor faults 981,154 to 23,171.
Different interior lengths and a single pair limit timing inference; published
points are not internal steps. The 120-second diagnostic does not close the
canonical failure; the full run above does. Why affine retention exposed the
clone's allocator costs remains unproven, while removing that clone has direct
ownership, behavior, timing, and cohort evidence. Frozen artifacts are under
`dense-lu-owned-review/frozen-0700a587f/` and `ims-dense-lu-ab-review/`.

The remaining thyristor raw loss has a separate one-pair diagnostic: ordinary
`2a0ea3aa` versus `af33fc5b8`, matching pinned-core/Rayon/allocator settings,
11.8055 versus 12.0707 seconds. All 160 channels and 55,456 published samples
over 0–10 seconds are byte-identical, SHA-256
`ff1e3d6ad381d852765b3b11e57cd461a8557db887113b42f3a1b16fab0b7c26`.
Interior system CPU is 0.08 versus 0.07 seconds, minor faults 35,379 versus
35,539: no IMS-like surge. This single ordered pair does not separate the 2.25%
latency difference from variability or establish source causality. The canonical
raw loss stays open; the diagnostic is not a retry replacing it. Evidence is
`/tmp/rumoca-fluid-speed-bdf/target/thyristor-ab-2a-af33/`. The next measured
optimization target is SMPM_VoltageSource among the six missing high identities.

### Full cohort confirms PlanarFourbar recovery at 7fed0a809

Follow-up IMS_Start diagnostic reproduces a source-dependent slowdown:
ordinary frozen `2a0ea3aa` and `7fed0a809` workers, sequential on core 0 with
canonical Rayon/allocator settings, take 9.306 and 14.546 seconds. The complete
802-channel, 15,003-sample trajectories are byte-identical. A 120-second
diagnostic budget permits completion and does not close the canonical failure.
Interior system CPU rises from 0.04 to 3.09 seconds and minor faults from
22,372 to 981,952; differing window lengths and one pair limit attribution.
An unwind-enabled current-source 12-second perf capture reproduces the timeout.
Source review identifies additional clearing in owned Jacobian assembly and
changed retained-allocation lifetime as competing explanations; neither is yet
the proven kernel-cost owner. Fix this regression before further expansion.
Evidence is `/tmp/rumoca-fluid-speed-bdf/target/rawloss-ab-2a-7fed/`, including
`IMS_Start/perf-unwind/`; no extra capture substitutes for the failed gate.

The IMS allocation attribution required a correction before implementation.
Perf's DWARF chain chose a stale selected-residual/status return address from
saved stack memory. Main challenged it using disassembly; the worker then
validated actual RBP links in the raw sample records. Of 746 fault samples,
726 belong to `matrix.clone().lu().solve(rhs)` in shared projection scaling;
the other 20 belong to trace recording. The selected-residual address is outside
the active frame-pointer chain. ELF hash, recorded mapping inode, and load bias
match; an initial load-bias suspicion was explicitly retracted. This is an
unwinding error, not evidence of a generated allocation or native status error.
Corrected raw-stack evidence is `provenance-unwind-correction.md` in that bundle.
Having multiple decoded frames alone does not validate their caller relationship.

Main proposed consuming the locally owned scaled matrix in LU and rebuilding
the same scaled matrix from immutable inputs only for the existing SVD fallback.
Both workers approved the bounded direction, preserving square checks, sparse
policy, fallback permission/tolerance, RHS, and nonfinite-result behavior.
Implementation and timing validation remain pending; why affine retention
exposed the LU clone's page faults is not yet established.

`tier2-affine-storage-7fed0a809/` covers all 566 models at clean `7fed0a809`:
**207 raw completions, 190 compared, all 190 high, 17 tracked exclusions,
zero missing**. The official transition diff preserves all prior 189 high
identities and adds PlanarFourbar. Its 872 comparable channels have zero
deviations; canonical simulation takes 9.128 seconds. Full and focused traces
match the earlier complete diagnostic byte-for-byte, SHA-256
`863c9039b40734fcdb1e2f3eb411bfdb4090bf171e6e576b2f0d2f88faf1c3b7`.
The historical union is now **190/196 retained** (188/194 original), with
IMC_YD, SMPM_VoltageSource, SMR_DOL, UniversalConstraint, Engine1b, and Fourbar1
still timing out. Fluid capability expansion remains paused.

Two raw execution losses remain explicit: IMS_Start and
ThyristorCenterTap2Pulse_RLV_Characteristic completed in `2a0ea3aa` but time out
here. Both previously had comparator exclusions; neither is a high-parity loss,
but both require causal investigation. The fixed 13-member raw recovery roster
now has only PlanarFourbar completing. The full gate still fails coverage and
runtime ratchets. Intermediate package deltas are Media Flat +1 and
Electrical.Machines Solve +1; there is no whole-model compilation loss.
Artifacts: `affine-storage-tier2-diff.{json,md}` and
`affine-storage-preservation-audit.json` under the campaign artifact root.

Worker `16f744833` integrates as `7fed0a809`. Shared projection retains lazy,
initialized dense Jacobian storage per issued block under an exclusive lease
through affine solving, refinement, retries, and certification. Every use
recomputes derivatives; full clearing and native transactional publication stay
intact. Failed/declined providers, reentry, malformed ownership/dimensions, and
public-coordinate rollback retain strict behavior. Retained seed linearizations
keep independent matrices and factors. Memory persists per exercised block until
runtime drop; no solver plugin, numerical cache, or allocator policy changes.

Noether approved both direction and final frozen source. Main's preliminary
review caught allocation moving ahead of malformed-structure validation; the
candidate restores fail-fast ordering and adds a negative allocation control.
The old source fails the real refresh allocation-count regression on its second
call; final source reuses storage with fresh parameter-dependent derivatives.
Other controls cover poisoned storage, native partial failure, sparse/mixed
decline, borrow lifetime through dense retry, and failed certificates. Independent
Y/time freshness is not separately varied by the new tests; code forwards both.

Main passes 553 shared tests, two native source tests, seven coupled-refresh
source tests, three fixed-initial-boundary tests, simulation-layer clippy, and
formatting. One mistyped filter initially selected zero tests and is not counted;
the corrected filter passes three. The fixed canary retains nine compared/high,
zero skipped/missing, zero transitions. The unchanged original eight recovery
targets have two compared/high, zero skipped/missing, six timeouts; the strict
selected-target gate correctly fails after comparison. No retry replaced it.
Frozen proof and logs are in `affine-storage-review/frozen-16f744833/`.

### Historical preservation scope audit

The independent audit of retained historical full cohorts confirms that the
original frozen 194-member roster is accurate for its three source runs but
does not include every subsequent success. Clean full `7ebb651bb` independently
establishes NandGate and DifferenceAmplifier. `fluid-high-water-targets.json`
therefore records 196 preservation obligations without rewriting the original
roster. Latest full `2a0ea3aa` retains 189/196, with the same seven timeouts and
no additional missing identity found. Its comparison remains 189 high/189
compared, 19 tracked exclusions, zero missing. Older `7ddbadf` also records
NandGate high, but dirty metadata limits reproducibility; it is not needed to
justify either addition. See the [audit](preservation-roster-audit-2a0ea3aa.md).
Restoring those seven remains the priority before Fluid capability expansion.

### Latest full cohort: local optional-seed recovery

At clean `2a0ea3aa`, `tier2-stage-seed-2a0ea3aa/` covers all 566 models:
**208 raw completions, 189 compared, all 189 high, 19 tracked exclusions,
zero missing**. The official `stage-seed-tier2-diff` against `b685d26e3` has
no compared-set departure, band change, whole-model compilation loss, or raw
completion loss. IMS_Start and ThyristorCenterTap2Pulse_RLV_Characteristic
recover raw execution; both retain their existing comparator exclusions and
are not new high-parity models. Two of the original 13 raw recovery targets
now complete; eleven remain. The fixed preservation roster remains **187/194**:
the same seven models still time out. Intermediate package counts include
Media Flat -1 and Electrical.Machines Solve -1, retained in the diff rather
than described as unchanged. The full coverage/runtime gate still fails.
`stage-seed-preservation-audit.json` binds the clean commit and exact identities.

Worker `8b684b72` integrates as `0d9b08d0c`: on an allowed optional-seed failure,
restore the entire stage-entry coordinate and project the existing issued block.
This retains upstream stages and full block residual/correction/non-invalidation
checks. Typed value-projection nonconvergence preserves the original-coordinate
whole-plan recovery path; native, semantic, and allocation errors do not authorize
replay. Successful/empty seed behavior and sensitivity error classification stay
unchanged. Noether independently reviewed both direction and frozen delta.

Main caught a missing downstream exhaustive SimError conversion during source
integration, after all 546 shared tests passed. `2a0ea3aa` adds the explicit
conversion with the prior diagnostic category/text; optimizer conversion was
also inspected. The retained failed build is not a passing check. Subsequent
two prepared-execution source tests, seven coupled-refresh tests, three initial
boundary tests, strict simulation-layer clippy, and formatting pass. Worker
old-red controls fail five/seven on the prior production path; all nine final
controls pass. The original global-entry fixture incorrectly prohibited ordinary
Newton iterations; its corrected assertion checks the first entry and final
residual solution. Failed fixture logs remain visible in the frozen evidence.

At `2a0ea3aa`, the fixed canary retains nine compared/high with zero skipped or
missing, and no transitions. The unchanged original eight recovery targets
retain ArmatureStroke high and seven timeouts (one compared, zero skipped/missing).
Selected-target success correctly fails. No canonical retry was substituted.

A separate sequential PlanarFourbar diagnostic using frozen ordinary `msl-fast`
binaries measures **11.7585 -> 5.2740 seconds (55.15% less simulation time)**.
All 3,494 published channels and 501 samples over 0–5 seconds are byte-identical,
SHA-256 `863c9039b40734fcdb1e2f3eb411bfdb4090bf171e6e576b2f0d2f88faf1c3b7`.
Main independently verified hashes and used unchanged
`cargo xtask repo msl -- plot-compare --reuse-traces`: all 872 OMC-comparable
channels, full horizon, worst bounded normalized L1 `6.904e-5`.
The pair does not establish canonical recovery or internal step counts.

The actual canonical request differs from the diagnostic in output directory,
timeout, IR emission, and source-root path; both roots contain the same 4,213
library entries. More materially, the canonical harness runs four pinned worker
processes with Rayon=1 and allocator overrides, while the diagnostic runs one
sequential worker with Rayon=4 and no explicit affinity. These differences were
not independently measured and do not identify the precise timeout cause. Future
profiles must match canonical settings; gates and budgets remain unchanged.
See [planar-perf.md](planar-perf.md), frozen worker proof under
`stage-seed-review/frozen-8b684b72/`, and `stage-seed-planar-ab-review/` under the
campaign artifact root. Full-cohort acceptance is limited to the results above.

### Preceding cohort and IMS timing investigation

At clean `b685d26e3`, `tier2-prepared-refresh-b685d26e3/` completes all 566 targets:
**206 raw completions, 189 compared, all 189 high, 17 tracked exclusions, zero
missing**. There are no comparison-set departures or band changes versus
`7ebb651bb`, and no complete compilation transition. The fixed preservation
roster remains 187/194 with the same seven timeouts. However, IMS_Start loses
raw completion: previously 10.5476 seconds, now a 12-second timeout. This is a
regression obligation even though the model has a tracked comparator exclusion.
None of the original 13 raw-execution recovery targets completes in this run.
`prepared-refresh-tier2-diff.{json,md}` and
`prepared-refresh-preservation-audit.json` retain identities and provenance.
The package table additionally shows Electrical.Machines Solve +1; intermediate
stage change is distinct from whole-model compilation or simulation recovery.
The full coverage/runtime gate fails. No timing recovery or overall acceptance
is claimed; IMS_Start investigation takes priority over Planar expansion.

`b685d26e3` integrates worker `43b0d0e44`: validate immutable refresh plans at
preparation, then retain constant-time state/solver/slice extent checks. It covers
complete/value/stage plans and root/clock remainders, including sensitivity.
Noether independently approved the frozen source after checking mutable-partition
rejection before writes. Main passed 537 shared tests, two source execution
checks, and formatting. The old-red control records nine repeated structural
scans; the candidate records zero with nonlinear residual/sensitivity checks.
Eighteen malformed layout cases and six dimension/slice mutations are rejected.
No numerical cache, certificate, tolerance, or solver policy is changed.

The fixed canary has nine compared/high, zero skipped/missing, and no transition.
The unchanged original eight recovery targets retain only ArmatureStroke high,
with seven timeouts; the strict selected-success gate fails after comparison.
Official canary/recovery diffs and `prepared-refresh-integrated-tests.log` are
under the campaign artifact root. Frozen worker proof is under
`prepared-refresh-review/frozen-43b0d0e44/`.

Linux perf source attribution and its limitations are recorded in
[planar-perf.md](planar-perf.md). A subsequent read-only mapping shows a concrete
three-output Planar tensor assignment already has a grouped native stage; the
interpreted path follows complete-plan fallback after seed failure. That
competing explanation blocks an unproven expansion of singleton native admission.
The next Planar diagnostic is paused while a sequential old/new IMS_Start
measurement distinguishes changed arithmetic from timing/load. Diagnostic traces
will not replace the failed canonical run or manufacture recovery by retry.

That bounded IMS_Start source comparison is now complete. One sequential run
per version, with identical ordinary `msl-fast` settings and a 120-second
diagnostic budget, takes 9.4721 seconds at `7ebb651bb` and 9.4887 seconds at
`b685d26e3`. Old/new and the retained successful old canonical trace are
byte-identical: SHA-256
`3bc27a496ae17fbb7018f70365c001b9b065421d20b843b9936dfe1b70e1fae6`,
802 channels, 15,003 published samples, full 0–1.5 seconds. Main independently
verified both diagnostic trace hashes. These are published samples, not internal
step counts. One pair cannot prove the exact load cause or a statistical speed
guarantee; it does not reproduce a source-dependent slowdown of the canonical
magnitude or any trajectory change. The raw completion loss remains open.
`target/fluid-campaign/ims-ab-review/` retains the compact evidence; original
binaries, requests, traces and profile fingerprints are in the worker's
`target/ims-ab-7ebb-b685/`. No canonical retry, comparator change, or recovery
claim follows. The next bounded Planar seed-failure diagnostic may proceed;
there is no observed numerical regression requiring a speculative code rollback.

### Native residual batching, focused evidence

Integrated `32e020219` (worker `d5f466bc`) batches the existing compiler-issued
residual selection through one native table/register scope. Program order,
full aggregate output extent, bounds, native error identity, residual checks,
and solver policy remain unchanged. The adapter stages outputs and commits
only after success; an admitted error never triggers replay. It evaluates fresh
state, parameters, time, and tables on every call. This is execution overhead
reduction, not a semantic repair or cached-result policy.

Main reviewed the production delta and source/negative controls. Noether's
independent read-only review found no correctness blocker. One diagnostic
limitation remains: nonfinite reporting occurs after a successful batch, so a
later native error can suppress an earlier nonfinite diagnostic; the values
and failing result remain unchanged. No explicit new NaN/Inf batch test was run.
Main independently passed eight native-selection tests, eight shared grouped
projection tests, two source execution tests, and formatting. The worker also
records broad tests and strict clippy in the frozen evidence bundle.

The source old-red control proves missing batch dispatch, not incorrect old
arithmetic. An earlier fixture declined native execution and was inconclusive;
its corrected control records six admitted individual calls before failing the
batch assertion. Main independently verified the final patch equals the built
patch and the complete diagnostic trace has SHA-256
`907092f42afa8714fc376ff3e81a1515862841764cae994f5d453ff805f59b1d`.
A single release SMPM diagnostic falls from 43.374 to 40.969 seconds (5.54%).
Unchanged `cargo xtask repo msl -- plot-compare --reuse-traces` compares all
697 channels over 0–2 seconds against pinned OMC: worst bounded normalized L1
0.02577. No budget, tolerance, channel, or comparator change; the diagnostic
still exceeds 12 seconds and is not canonical recovery.

Tier 1 at clean `32e020219`: `canary-residual-batch-32e020219/` compares nine
models, all high, zero skipped/missing. Official `residual-batch-canary-diff`
reports no stage, completion, or band transitions. The unchanged original eight
recovery targets in `recovery-residual-batch-32e020219/` retain ArmatureStroke
high (one compared, zero skipped/missing); all seven other targets still time
out at 12 seconds. `--require-selected-targets-success` correctly fails after
comparison. No retry was made. The canary transition command's incremental
dev build overlapped the recovery run; timing is therefore not a controlled
performance comparison. Sampling uses a separate idle interval.

Frozen evidence: `target/fluid-campaign/residual-batch-review/frozen-d5f466bc/`;
main checks: `residual-batch-integrated-tests.log`. This focused evidence precedes the full `b685d26e3` cohort reported above.
Fluid expansion remains paused.

The user has prioritized recovery of all eight previously high-parity models
before Fluid expansion. Group failures only when producer evidence establishes
a shared cause. General compiler and shared Model Exchange repairs, reviewed
focused regressions, and per-model preservation remain the acceptance criteria.

The preceding full 566-model run is `tier2-progress-projection-7ebb651bb/`, at
`7ebb651bb`: **207 raw completions, 189 compared, all 189 strict-high, 18
tracked exclusions, zero missing**. The full gate still fails coverage/runtime.
DifferenceAmplifier and NandGate enter strict-high; IMS_Start recovers raw
execution and remains a reviewed exclusion. No prior completion, compared
model, or high band is lost. `progress-projection-tier2-diff.{json,md}` retains
the exact transitions, including intermediate stage changes. The original
194-model preservation roster remains 187/194: all seven outstanding members
still exceed the canonical budget. The two new high models do not offset those
losses and must remain visible in subsequent full-cohort transition checks.
IMS_Start is one of the 13 historical raw-execution recovery targets; twelve
of that fixed roster remain unrecovered.

Two general changes are accepted through focused and cohort validation:
`a27611b4e` (worker `9b9811b09`) gives both numerical adapters the shared host's
existing representable-time progress bound, removing arbitrary absolute step
floors without changing error tolerances. Seven analytical startup cases,
529 shared tests, and 11 BDF/6 RK tests pass independently. SMR_DOL's startup
failure is removed, but it still times out canonically; its longer diagnostic
has no deviation-level channels under the unchanged xtask comparator.
`7ebb651bb` (worker `202b5146`) removes a veto that discarded source-bound
compiled projection Jacobians whenever interpreted reverse AD was available.
Independent source, 532 shared-runtime, and nine native-Jacobian tests pass.
A release SMPM diagnostic falls from 54.956 to 43.374 seconds with an identical
full trace (SHA-256 `907092f42afa8714fc376ff3e81a1515862841764cae994f5d453ff805f59b1d`).
It remains outside the canonical 12-second limit; no SMPM recovery is claimed.

The fixed canary retains nine high traces, zero skipped/missing, with no
transitions; the unchanged eight-model recovery focus has ArmatureStroke high
and seven timeouts. Evidence is `progress-projection-canary-diff.{json,md}`,
`progress-projection-recovery-diff.{json,md}`, the tracked
[SMR startup proof](smr-startup-499.md), and immutable bundles under
`target/fluid-campaign/{smr-progress-review,prepared-projection-review}/`.
The final release profile corrects the earlier torn-Jacobian hypothesis: SMPM's
hot blocks take the affine path. Further work now profiles residual batching;
a separate bounded PlanarFourbar profile distinguishes step count from step cost.

The previous full checkpoint `499b5533fcf2d4d319bce9f80ab2c8e6f898a9d9`
restored ArmatureStroke to strict-high: 204 raw completions, 187 compared, all
187 strict-high, 17 tracked exclusions, and zero missing comparisons. The
full gate still fails coverage and runtime floors. Independent set comparison
against the fixed 194-model preservation roster leaves exactly seven absent
models: IMC_YD, SMPM_VoltageSource, SMR_DOL, UniversalConstraint, Engine1b,
Fourbar1, and PlanarFourbar. The separate 13-model raw execution recovery roster
is unchanged. Fluid work remains paused.

`target/fluid-campaign/initial-boundary-tier2-diff.{json,md}` compares the full
`tier2-package-initial-767151bdc/` and `tier2-initial-boundary-499b5533f/` runs:
ArmatureStroke near-to-high is the only band transition; no model leaves the
compared set and there are zero raw simulation or complete compilation
transitions. Intermediate stage results do change: HeatExchangerSimulation
moves from ToDae ED019 to a Flatten timeout; Inverse_sh_TX moves from ToDae
ED019 to Flatten EF015. Both already failed full compilation. These changes
remain visible; no causal explanation or retry-to-pass is credited. Package
Solve count rises by one while total Flatten count falls by two.

The general ArmatureStroke repair is `c8a57c038` plus `f74142c85` (worker
`7e265347` plus `921fa1c2`). Source fixes the initial position exactly at a
strict contact boundary. An unrelated initialization row requiring algebraic
reconstruction globally disabled direct assignments, leaving a tiny position
residue that selected the wrong contact branch. Runtime now uses the existing
compiler-owned row role to permit exact assignment for the eligible row and
checks the settled residual. It retains the tighter source-equation settlement
policy; ablation reproduced the exact old and failed traces before the repair.

Independent review found a second path: an already-within-tolerance seed could
skip the assignment. The worker reproduced the requested near-boundary source
case as old-red; the follow-up moves the existing full-residual singleton
assignment before convergence acceptance and reuses the existing guarded
improvement rule. No tolerance, row owner, residual or coordinate certificate,
solver adapter, model-name branch, trace sample, or comparator policy changed.
This does not claim exact roots for arbitrary nonlinear initialization systems.

Independent final validation passed three source fixtures exercising both RK
and BDF, all 527 shared-runtime tests, and formatting. Worker strict Clippy and
27 existing initialization tests also passed. The final fixed 20-model canary
compares nine traces, all high, zero skipped/missing, with zero transitions;
see `initial-boundary-canary-diff.{json,md}`. The focused eight-model recovery
run has only ArmatureStroke completing and comparing high, zero skipped or
missing; the other seven failures remain visible. ArmatureStroke closure is
confirmed by the complete Tier 2 run above, not inferred from the focused run.

Evidence is retained in `target/fluid-campaign/armature-stroke-767-review/`,
including immutable `frozen-7e265347/` and `frozen-921fa1c2/` bundles. Independent
hashing confirms byte-identical ArmatureStroke compiler IR through Solve at
456 and 767. The unchanged comparator on native OMC-step diagnostics exposes
sampling sensitivity in the original L_stat and stopper-power discrepancies;
those diagnostics changed no canonical reference and are not the closure proof.
The original 767 failed run remains unchanged and auditable.

A later API review corrected the earlier dual-solver test claim: the
`simulate_dae` helper is a BDF alias and ignored the fixture's solver selection.
Both initialization fixtures now call `simulate_dae_with_diagnostics`, the
actual dispatcher. Main reran all three boundary cases and the initial-output
case successfully with both solver modes on the accepted runtime fix, before
integrating the time-progress change. Earlier failure evidence remains valid
for BDF only. The corrected log is `actual-solver-dispatch-tests.log` in the
ArmatureStroke review directory; canonical MSL evidence is unaffected.

Noether now owns SMR_DOL's time-zero failure. Galileo owns the measured SMPM
certified-refresh performance cost. The [seven-model census](preservation-census-767.md)
retains exact previous-good and current failure evidence; six timeouts are not
assumed to have one cause merely because they share a budget failure. Main
independently verified all 59 evidence hashes: 58 at their retained paths and
one moving-worktree roster against its frozen census commit `cef98db13`.

Two changes reached Tier 1 before that full gate:

- Package-owner constants (`e4ee7e6e7`, corrected by `503491496`, with additional
  inheritance proof in `767151bdc`) now separate canonical package constants
  from modified component occurrences. Source fixtures prove 4184 for the
  selected water package, isolation from a component modified to 900, inherited
  owners, and a derived dependency overridden to 2500. EmptyTanks advances from
  ED019 at `tank1.h_start` to ED008 at `tank1.heatTransfer.states.p`; this remains
  a compilation failure, with zero new Fluid simulation or parity successes.
- Shared Model Exchange initial settlement (`0fe9eefed`, worker `bb2406c2`)
  uses the existing algebraic settlement precision for source initial equations.
  A regression exercises both RK-like and BDF paths. It removes SMPM's retained
  nonzero controller-output initialization residue. Canonical SMPM still times
  out at 12 seconds and SMR_DOL still fails at minimum step; neither is recovered.

Independent checks passed: 656 flatten tests, 527 shared-runtime tests, the
initial-output regression, formatting, and strict Clippy. The fixed 20-model
`canary-package-initial-767151bdc/` run compares nine traces, all high, with zero
skipped/excluded/missing; `package-initial-canary-diff.{json,md}` records zero
transitions against `canary-zero-rhs-456f1619/`. These are Tier 1 results and do
not override the failed full gate.

Review caught a real producer ownership gap in the original package candidate:
component-context values could enter the shared package table. Source tests
reproduced that leak before the correction. The reviewer did not establish a
wrong final folded value; its inherited-owner prediction of default 1 instead
produced an unresolved reference. The orchestrator's separate stale inherited
constant hypothesis was disproved by the executable 2500 fixture. Those claims
are corrected rather than counted as additional confirmed defects. A mixed
modified-occurrence callable remains a typed MissingSourceContext refusal.

Frozen package patches and producer evidence live in
`target/fluid-campaign/empty-tanks-owner-candidate-review/frozen-8af908b5/` and
`frozen-e0bea270/`; corrected real-model IR is in
`empty-tanks-owner-correction-review/`. Only `numeric-before-test-only.log` is
valid old-red numeric evidence; two earlier transplants failed to compile.
Initial-settlement evidence is in `initial-settlement-review/`. Its separate
SMPM long diagnostic completes in 54.95 seconds and the unchanged official
comparator finds all 697 channels high over 0–2 seconds. This larger-budget
diagnostic is not a canonical execution recovery or a cohort parity claim.

On the user's request, difficult implementation work moved from Luna to
`gpt-5.6-sol` at high effort on 2026-09-22, approximately 12:27 UTC. Their worktrees are `/tmp/rumoca-zero-rhs` and
`/tmp/rumoca-fluid-speed-bdf`; their current assignments are SMR_DOL startup
and SMPM refresh performance above. Both reuse existing build
targets and received compact handoffs; the prior Luna implementers and review
worker were closed. The orchestrator still owns independent acceptance review.

Evaluate this change by time to a validated fix, substantive review corrections,
avoidable build reruns, and verified patches. No reliable per-worker token-cost
measurement is available. Sol has delivered the Tier 1 changes above, but
review required substantive correction and the full gate remains failing;
these results do not establish comparative cost efficiency. The preceding phase required correction of wrong workspace
builds, a cold nested target, Nix-overridden concurrency settings, stale Cargo
artifacts, and an incorrect observable-to-equation mapping. Preserve these
failures as evidence; they are not model regressions or successful diagnostics.

The SMPM startup mapping is corrected: observable ordinal 740 is
`currentQuasiRMSSensor.ToPolar1.y[2]`, visible index 740, loaded from Y871.
The inventory filters visible names for continuous Real metadata; program
indices and Y indices are different coordinate spaces. The prior claim that
this was a formal voltage-sensor current derivative is retracted, including
the resulting proposed zero-current cancellation diagnosis. The old agent
`smpm-channel-740-mapping.md` is invalid. Corrected mapping, original IR and
result hashes, and the bounded diagnostic ledger are frozen under
`target/fluid-campaign/smpm-observable-740-review/`.
Newton converges and state LTE decreases while the angle's supplemental
observable score increases at small steps; actual simulation fails in
0.029604882 seconds with minimum-step exhaustion. This is diagnostic evidence,
not a canonical recovery or proof of the responsible arithmetic defect.

Sol's subsequent bounded probes confirm that the angle inputs match the
independent current states to roundoff. At the fixed initialized state, the
shared ME RHS returns current derivatives `1.80178636115795790e-6` and
`1.85167683215346735e-5`, unchanged for sampled time offsets from zero to
16 ns. The source fixes the initial currents to zero but does not explicitly
fix their derivatives. Whether these derivatives follow from the source
equations or numerical cancellation remains unresolved. A nearly radial LTE
perturbation of currents scaling approximately with step size squared changes
the angle increasingly as the step shrinks; this is estimator evidence, not
an independent trajectory-error measurement. No channel exclusion, tolerance
floor, or production fix is justified by these probes. Galileo continues with
the initialized source equations and the first numerical RHS producer.
Probe patches, commands, source/worker/IR hashes and results are recorded in
`/tmp/rumoca-fluid-speed-bdf/target/smpm-startup-ledger-456f1619/angle-input-findings.md`.
Both completed probes were removed from the worker's source tree.

EmptyTanks's current Flat IR shows both selected enthalpy function instances
still referring to unbound `PartialSimpleMedium.cp_const` instead of the water
package's `cp_const=4184`. The evaluator's qualified-name-to-enum fallback
explains the misleading `Enumeration * Real` error. The selected-owner repair
is the held candidate above; no numeric coercion or model substitution is accepted.
The original Flat IR and request/result are frozen with hashes under
`target/fluid-campaign/empty-tanks-producer-before/`. A test-only Luna draft has
no valid test result and is not accepted as proof.

TwoMass review at production commit `456f1619` is closed as a reasoned
pointwise-oracle boundary, not strict-high. The source's `DoubleRamp(offset=0,
height_1=1,height_2=-2)` has initial zero flow through 0.2 s; the previously
suggested [0.4,0.6] drop-out interval belongs to a different model and is
rejected here. At each three-way FlowPort junction, zero flow removes connector
enthalpy from `semiLinear` energy-flow equations. No fixed connector enthalpy
initialization selects between OMC's 288.15 and Rumoca's zero. All five physical
states have identical fixed initial values. `TwoPort.tapT=1` reduces `T_q` to
the physical state temperature, so the free aliases do not drive heat exchange.

The complete trace has 100 high and 17 deviating channels. The 17 are
`dTCoolant1/2`, `pipe1/2/3.dT`, `pipe1/2.T_a/T_b`, `pipe3.T_a`,
`pipe1/2.flowPort_a/b.h`, `pipe3.flowPort_a.h`, `pump.T_b`, and
`pump.flowPort_b.h`. Full-horizon state errors are at most 0.0017260817 K;
heat-flow errors are at most 0.0034521634 W. These bounds alone do not prove
agreement of the other observables.

The orchestrator rejected a proposed additional near-zero-flow boundary:
both traces have finite flow about 1.33e-15 at t=0.7, and the OMC grid contains
an additional right-side sample absent from Rumoca's grid. No interval around
that crossing is excluded. Instead, lossless suffix copies beginning at 0.201 s
were compared with unchanged `cargo xtask repo msl -- plot-compare
--reuse-traces`. All 117 trajectory channel scores are below 0.001115, versus
the existing 0.05 high threshold; mean bounded L1 is 0.0001605. This suffix
includes the reversal and preserves all original samples after the cut.
It is diagnostic evidence for the initial-gauge diagnosis, not full-model
certification. Linnaeus independently approved this narrower rationale and
retracted both the near-zero exclusion and a blanket typed-mask requirement.

Evidence is `target/fluid-campaign/twomass-pointwise-456.{html,log}`,
`twomass-window-audit-456.json`, and
`twomass-postinitial-diagnostic/{provenance.json,compare-valid.log,plot.html}`.
Provenance retains original and derived trace hashes. The first suffix-copy
attempt rejected an incorrect matrix-layout assumption before producing
traces; only the corrected variable-major copies were compared. Original
canonical results remain unchanged: 187 high / 188 compared, 16 excluded,
zero missing, one recorded deviation. The newly tracked exclusion affects
future comparisons and keeps TwoMass non-high; it manufactures no recovery.

Latest user direction narrows the next Fluid milestone to exactly
`Modelica.Fluid.Examples.Tanks.EmptyTanks` with end-to-end strict-high OMC
agreement, and requires preservation of every main/earlier campaign success.
The fixed preservation roster contains 194 models; seven currently need
recovery. Current-main CI has 191 strict-high / 191 compared, 21 excluded and
zero missing; the latest candidate has 187 strict-high / 188 compared,
16 excluded and zero missing. Six main models are lost despite two gains.
The campaign ledger records exact provenance and the official xtask diff.
This regression is not accepted or hidden by aggregate gains.

Earlier Luna assignments and preliminary hypotheses below are historical.
The active owners and evidence above supersede them. TwoMass disposition is
closed; EmptyTanks and preservation remain the only implementation streams.

The user reaffirmed that the non-Fluid counterexample must be fixed before
Fluid expansion resumes. Two implementation agents are active; independent
reviewers activate at concrete checkpoints.

The user additionally requires production fixes through the shared Model
Exchange interface for all solvers. Solver-specific probes remain diagnostic.
The post-Newton safeguard candidate leaves the original trace byte-identical;
it is not a fix for this counterexample. Exact-time reference sampling also
finds comparable feedback error at accepted endpoints and interior samples,
so an interpolation-only explanation is rejected.

A shared observation-time step cap still produces deviation channels and is
rejected as the accuracy repair. That experiment exposed a separate ME
continuation defect: native time `0.00020000000000000042` is normalized to
public time `0.0002`, within the existing roundoff bound and with identical
states, but the following continuation rejects bitwise time inequality.
An independent retained-backend regression fails on the old source and passes
with a shared public/native receipt. Review then found cross-zero sampling
and public-progress defects in the first receipt patch. Its revision is
preserved, unintegrated, at
`/tmp/rumoca-fluid-speed-bdf/target/receipt-review-candidate-v2.patch`.
The speculative stale-event objection was explicitly retracted after tracing
the successful event-restart path.

The active prototype adds a host-owned observable-error obligation through
standard ME projections while retaining the existing state acceptance test
and canonical tolerances. It must cover every published continuous Real
channel, including local algebraics, restore component state transactionally,
and preserve typed failures. The numerical plugins may consume an opaque
common capability; they may not acquire compiler IR or model-specific policy.
This is a design under validation, not a counterexample closure or an accepted
production change.

The observable-norm implementation checkpoint has common ME projection plus
generic BDF and RK45 consumers. Orchestrator review required the BDF supplemental
norm to be squared before joining its existing squared error controller, the
Newton check to use its existing nonlinear budget, and RK45 to pass the
correction from its high-order state to its embedded alternative. Linnaeus
conditionally approves the corrected numerical wiring; final digest review and
regression evidence remain pending. A proposed BDF coefficient-index objection
was retracted: the existing state branch correctly uses `order - 1`.

The first model attempt stops during setup because the prototype incorrectly
requires an explicit nominal for each observable. [FMI 3.0.2 section 2.4.4,
Table 15](https://fmi-standard.org/docs/3.0.2/#type-definitions) defines a default
nominal of one when no other information is available.
The common owner is correcting that handling, removing role/causality filters
from the published continuous-Real inventory, and proving component restoration
through the real getter on success, error, and unwind. This failed attempt is
not evidence about trajectory accuracy. Source audit of the active numerical
workspace `/tmp/rumoca-speed-runtime-67978` finds only the intended host and
numerical-hook production changes; the earlier unrelated source contamination
is absent. The shared host capability is now integrated as a validation
checkpoint; the numerical consumers and counterexample closure remain pending.

The completed observable prototype subsequently simulates SpeedControlledDCPM
with unchanged `rtol=1e-6` and `atol=1e-10`. The unchanged production
`plot-compare --reuse-traces` comparator against the original Tier 2 OMC trace
reports 352 channels: its four worst channels are minor and the fifth is high,
establishing 348 high, four minor, and zero deviation channels. This meets the
model-level strict-high rule. Evidence is retained in
`target/fluid-campaign/speed-observable-diagnostic/`, with trace SHA-256
`09ef129f80e3362b94911df792239981c5c1fe2b6f4faf95f9085dabfb5e1109`.
The run uses a development build and 60-second diagnostic phase budgets;
it is neither a canonical gate nor cohort evidence. Simulation execution takes
1.851214441 seconds. Final review requires the failure-path correction that
latches caught projection panics, a generic hook that cannot silently discard
a supplied error obligation, and a numerical-budget regression. Canonical
focused, fixed-canary, and complete Tier 2 validation remain pending.

Common host revision 4 has independent Linnaeus approval at patch digest
`c96576bacf752691f597aa54d379c33082d7359c6e538c5e3b6b146d8f88a746`.
Central validation passes all 185 `fmi_me` tests; the owner also passes strict
clippy. The numerical analytic regression now fails on the observable budget
under ablation and passes with the hook. Numerical review remains held because
an observer rejection replaces the pending Newton correction while the
backtracking line search retains its previous norm. Linnaeus independently
confirms that mismatch. A correction must refresh that pair without resetting
convergence history and validate both line-search variants before another
actual-model run. This checkpoint does not mark the capability complete.

The final numerical patch is independently approved by Linnaeus at digest
`3a2ee37fe0caa600e383a956db14820a441f9c11f1dcbb5bee2ad09406f21ab7`.
It refreshes Backtracking's cached norm for the fresh pending correction
without resetting convergence history. Its three-step behavioral fixture
converges to `x=0.912` with the refresh and falsely diverges at `x=0.92` without
it. Separate analytic observable-budget regressions pass for both line-search
variants and fail numerically when the callback is disabled. Unsupported
nonlinear methods now return a typed refusal. Central integration passes
11 Diffsol tests, all six RK45 tests, formatting, and strict clippy across the
three affected solver crates; exact patches and
evidence are retained in `target/fluid-campaign/observable-final-review/`.
An independently approved extraction of the unchanged RK45 observable-error
calculation into a helper closes the central function-length lint failure.
The ME-INT-007 wording now explicitly distinguishes the actual trial iterate
from the pre-call component snapshot; the implementation is unchanged by that
clarification. Canonical model, canary, and cohort validation are still pending.

Canonical validation at clean `7be19ca6cc00b329cd9ce6179dcf7c21962ddb71`
confirms SpeedControlledDCPM is strict-high: one compared model, zero skipped
or missing, 348 high channels, four minor, and zero deviations. The fixed
20-model canary has nine compared traces, all high, zero skipped/missing,
and no transitions against `canary-after-record-index-rev2`. Evidence is
`speed-observable-canonical/`, `canary-observable-7be19ca6/`, and
`canary-observable-diff.{json,md}` under `target/fluid-campaign/`.

The complete 566-model Tier 2 run at that same commit **fails**, so the combined
fix is held. `tier2-observable-7be19ca6/` has 196 raw simulation completions,
178 compared traces, all strict-high, 18 skipped, and zero missing traces.
`tier2-observable-diff.{json,md}` records 20 prior completions lost and one
gained; 16 previously high models leave the compared set. Fourteen of those
16 time out, SMPM_VoltageSource exhausts the step size at time zero, and
UniversalConstraint fails algebraic coordinate convergence during projection.
Runtime speedup medians also fail the quality gate. PrismaticConstraint
completes its one budgeted attempt and compares high; this observation does
not establish an independent performance repair for its earlier timeout.

No excluded model, changed tolerance, retry, or reduced coverage closes these
regressions. Two bounded investigations now own the added Rectifier runtime
cost and the SMPM_VoltageSource/UniversalConstraint contract failures, using
exact `7be19ca6` snapshots. The original Speed counterexample is repaired in
this run, but the candidate cannot be accepted and Fluid expansion remains
paused until the regressions are resolved.

The bounded Rectifier ablation retains observable projections but returns zero
only for the diagnostic admission score. It reduces advances from 100,758 to
11,184 and observable evaluations from 284,141 to 22,380. Projection cost per
evaluation remains approximately 94 microseconds; added steps dominate this
regression. The real first-5,000-call histogram identifies published channel
124, `$derivative.Inductor3.i`, as the strongest trigger. Logs are retained in
`/tmp/rumoca-observable-performance-7be19ca6/rectifier-profile-v2.log` and
`rectifier-ablate.log`. These instrumented development timings are diagnostic,
not canonical gate results. The ablation is not a proposed production change.

Independent direction review confirms that ME-INT-007 adds an all-published
continuous-Real obligation beyond the ordinary state tolerance contract.
Generated-variable origin currently does not survive into the FMI observable
inventory. Investigation must establish each channel's typed producer and
local sensitivity before proposing a correction; spelling-based exclusions
cannot establish whether a channel is an internal coordinate or a source
observable. SMPM_VoltageSource's annotation sets both tolerances to `1e-6`;
the earlier suspected tolerance mismatch was checked and retracted.

Further adversarial review identifies two numerical defects independently of
any model names. BDF's current-step norm includes the shared observable score,
but its lower/higher-order predictions omit it, ranking candidate steps under
different metrics. Also, the existing native state-error branch uses the
previous order's NDF coefficient. The coefficient constructor and the
[reference BDF implementation](https://raw.githubusercontent.com/scipy/scipy/main/scipy/integrate/_ivp/bdf.py)
both index the current correction by its current order. The earlier objection
that the new observer disagreed with the native branch remains retracted;
this new finding is that the native branch itself has an indexing defect.
Linnaeus independently confirms both findings. A generic numerical correction
using the same opaque ME callback is under focused validation, not yet accepted.

SMPM startup instrumentation rules out stale BDF initial derivatives: the
stored derivative equals a fresh ME evaluation exactly. Its first implicit
step nevertheless changes the two current-state slopes substantially. The
remaining distinction is time-dependent forcing versus state/projection
response; no initialization fix is proven yet. Evidence is retained in
`target/fluid-campaign/triage-smpm-startup-proof-rhs/stderr.log`.
For UniversalConstraint, the final diagnostic selects the largest remaining
residual across all rows. That row does not identify the block whose coordinate
certificate failed; a repair based only on the reported row is unjustified.

The BDF order/coefficient correction has independent Linnaeus approval at
patch SHA-256
`887b087597b99adf5ddc37e68d2c6241575069ccd140861ef093126ded2634fe`.
The nonlinear observable fixture changes the selected candidate order, while
a zero observable preserves the state-only factors. Independent coefficient
checks cover orders one through three. Restoring each defect separately
produces its expected assertion failure; the restored candidate passes.
The proposed post-step failure objection was retracted after tracing the
common host's existing failure latch, restoration, and terminal session-loss
path. Exact sources, logs, and the verification manifest are retained in
`target/fluid-campaign/bdf-order-review/`. Integration verified the old file
byte-for-byte and the approved new source digest before copying it. Central
checks and canonical model/canary validation are pending.

Central validation initially finds one failing stiff-voltage fixture. It uses
raw current-only BDF error control while asserting accuracy of the amplified
voltage. Its repair supplies the same generic observable capability and exact
nominal-aware ME normalization as production, retaining both state tolerances,
the `0.02` hard stop, and the `1e-5` analytic voltage assertion. Linnaeus approves
the test-only source at SHA-256
`67f58eb9568a3de9038956cc112780771667794a43828431019509f96213531a`.
The owner passes all 11 Diffsol tests, formatting, and strict clippy. Disabling
the dedicated hard-stop Jacobian refresh still passes under the stronger
observable check and other refresh paths; this is not an isolated proof that
that refresh is necessary. No assertion is relaxed to manufacture a failure
or pass. The fixture proves observable accuracy and the clipped endpoint.
Exact review evidence is retained in `target/fluid-campaign/bdf-order-review/fixture/`.

Central validation of that exact integrated correction now passes all 11
Diffsol tests, workspace and vendored formatting, and all-target strict clippy
for the affected adapter. The focused canonical Speed/Rectifier comparison
and fixed canary remain pending; the full-cohort regressions are still open.

Canonical focused validation at clean `15681d64` reopens the Speed counterexample.
`target/fluid-campaign/bdf-order-focused-15681d64/` compares both requested models,
with zero skipped and zero missing traces. Rectifier has 112 high channels and
zero deviations. SpeedControlledDCPM has 348 high, two minor, and two deviation
channels: the saturation/anti-windup pair has maximum bounded error
`0.22255483386614067`. The correction remains held; this result cannot be
accepted because Rectifier recovered. The next review concerns the signed
error vector supplied to nonlinear observable projection. No coefficient,
tolerance, safety factor, or channel selection is changed to fit the band.
Canary and broader gates are deferred until the focused counterexample closes.

The signed-correction audit identifies a separate generic error: BDF supplies
numerical-minus-exact error to a callback that adds its argument to the trial
state. Both current and candidate projections must negate that error. The
first analytic fixture was rejected because its NDF startup formula was wrong
and its exact observable error did not exceed the declared budget. The corrected
fixture uses `y'=y`, predictor `1.1`, NDF correction `0.00921658986175115`, and
exact solution `exp(0.1)`. With a fixed saturation boundary of `1.108` and budget
`1e-4`, the exact observable error is `0.0028290819243523835`: the positive-sign
projection misses it, while the negative-sign projection detects it.

Linnaeus approves the complete corrected source at SHA-256
`e78d31cebb760cf7108ec7d6f32a07930f5cbe58ce8f52b950de1ca45aac099e`,
including semantic old-red/restored-green evidence and the common contract's
sign clarification. Main verified its preimage and integrated the frozen
source; the complete integration diff has SHA-256
`aaefcaee15a92e341d8c85e381c38a1fa19588e66383673363520f3c911db1e3`.
Evidence is retained in `target/fluid-campaign/bdf-sign-review/`. The broader
vendor log contains 29 passes and 16 historical snapshot mismatches; it is not
a full-suite pass and is not the focused sign proof. A retained signed Speed
diagnostic still has two deviation channels, so this numerical correction is
not counterexample closure. Its full-source run provenance is being corrected
to distinguish the runtime source from subsequent test-only changes; central
validation and canonical measurement are pending.

Run provenance is now corrected: the retained signed-model run used verified
pre-fixture source `83e27f751283782f317abbd9ec39d5e1fbf88a37fdec8028b3f43ec2d1d5d9cd`;
subsequent test-only corrections produced the reviewed `e78d31ce` source.
Central validation passes all 11 adapter tests, workspace/vendor formatting,
and strict all-target clippy. Two assertion-wrapping changes apply rustfmt
without changing the reviewed behavior. The canonical focused comparison is
next; the sign fix still makes no Speed counterexample-closure claim.

Canonical validation at clean `c4aa3e3630e2e50c6fd56f2de978ab5773c92101`
confirms that limitation. `bdf-sign-focused/` compares two models with zero
skipped/missing traces: Rectifier has 112 high channels; Speed has 348 high,
two minor, and two deviations, with maximum bounded error
`0.22247994314679026`. The official xtask transition report
`bdf-sign-focused-diff.{json,md}` records no coverage or agreement-band
transitions against the same two-model `15681d64` run. The small numerical
score change is not a closure. The remaining investigation compares nested
interval refinements from identical solver and FMU snapshots to distinguish
local estimator behavior from accumulated trajectory error. Broader gates
and Fluid expansion remain paused.

The nested-refinement diagnostic produced no measurements: its single bounded
attempt called a directional derivative outside the ME activation window.
No branch report exists, so it supports no endpoint or global-error inference.
The temporary diagnostic is parked outside main.

A separate contract audit identifies an unjustified choice in the newly added
observable policy. Both existing state adapters scale the absolute tolerance
by nominal, whereas the new observable formula inserted nominal as a relative
floor. Near zero, that changes the meaning of a declared `atol=1e-10` to a
budget of about `1e-6` at `rtol=1e-6` and default nominal one. The repair aligns
the shared observable check with the existing state-LTE scaling:
`clamp(atol * nominal, MIN_POSITIVE, MAX) + rtol * max(abs(actual), abs(trial))`.
[FMI 3.0.2 section 2.4.4](https://fmi-standard.org/docs/3.0.2/#type-definitions)
describes nominal-based absolute scaling; the exact formula here comes from
the existing Rumoca adapters. Endpoint/root consistency remains a separate
host obligation. No declared tolerance or comparator band changes.

The nominal `0.01`/`3` boundary tests and consistent unit-rescaling test fail
under the old formula and pass under the corrected one. Their analytic
projection and independently chosen boundaries do not depend on MSL outcomes.
Independent Linnaeus review approves patch SHA-256
`e90932bbca16fb33763e7daf33f019615e11ec354467976eade73dc8808f7eb8`,
including preserved typed failures, finite clamping, and the aligned synthetic
voltage fixture. The owner passes 189 shared ME tests, 11 Diffsol tests, four
filtered RK45 tests, formatting, and strict all-target clippy. Main verified
all preimages and retained hashed evidence in
`target/fluid-campaign/observable-scaling-review/` before integration. Central
checks and canonical model evidence remain pending.

Central validation of the scaling correction passes 189 shared ME tests,
all 11 Diffsol and all six RK45 tests, formatting, and strict all-target clippy
across the three solver crates. Canonical focused validation is next.

At clean `608ee8b9a835e7d3235d8e05817b29caa0b590fd`, canonical
`observable-scaling-focused-608ee8b9` compares both fixed targets, with zero
skipped or missing traces. SpeedControlledDCPM is strict-high (348 high,
four minor, zero deviation channels; maximum bounded error 0.194965934),
and Rectifier has 112 high channels. Their simulation times are 0.7285 s and
3.1069 s. The fixed `canary-observable-scaling-608ee8b9` compares nine models,
all strict-high, zero skipped/missing, with zero transitions against the
previous fixed canary. These are Tier 1 results, not cohort numbers.

The complete 566-model `tier2-observable-scaling-608ee8b9` at that same clean
commit records 202 simulation completions: 185 compared, all 185 strict-high,
17 skipped, zero missing traces, and zero near/deviation models. This is
185/566 (32.69%) strict-high cohort coverage. The originating Speed model
remains strict-high with the same channel counts. Its counterexample is
closed by a general shared ME fix at unchanged declared tolerances; this
is not overall campaign completion.

The full quality gate still fails coverage and runtime. Against `67978a5d`,
`transition-diff` reports nine formerly high models leaving comparison,
one entering, and Speed moving near to high; raw simulation completion loses
14 models and gains one. Against `7be19ca6`, nine models enter comparison and
two leave: OvervoltageProtection (12 s timeout) and Vehicle (minimum step at
4.999999999999962). The other seven formerly high departures are IMC_YD,
SMPM_VoltageSource, SMR_DOL, UniversalConstraint, Engine1b, Fourbar1, and
PlanarFourbar. The runtime system-speedup median is 1.455393, below the
2.259809 floor. All failures remain in scope; no baseline is promoted.
Exact transition evidence is `tier2-scaling-vs-baseline-diff.{json,md}` and
`tier2-scaling-vs-observer-diff.{json,md}` under `target/fluid-campaign/`.

Two bounded implementation lanes now investigate the current Vehicle
boundary failure and OvervoltageProtection performance regression. Semantic
repairs must remain in shared Model Exchange; numerical plugins may consume
opaque capabilities but may not interpret models, published channel roles,
or FMI state. Independent review must establish the responsible producer
before accepting a patch. Fluid expansion remains held while these coverage
and runtime regressions are repaired.

The explicit solver-neutrality audit passes all 12 `fmi_me_boundary` tests
and `solver_backend_boundary::concrete_solver_backends_consume_the_me_contract_only`
on production `608ee8b9` (documentation checkpoint `953a3530`). Both adapters
consume the same opaque observable-error handle. Logs are
`me-boundary-608-tests.log` and `solver-backend-boundary-608-tests.log`.

Current Vehicle diagnostics locate an unpublished table event: near `t=5`,
the host has no cached time event and supplies hard stop `60`, despite an
already accepted native interval ending at `4.999999999999962`. At initial
event return, the table's scaled deadline is `5`, while its dependent actual
deadline remains the old value `0`, and the ME result publishes no next event.
This points upstream of numerical stepping; it does not yet decide whether
guarded-equation lowering or common event settling is the first broken
producer. The isolated evidence is under
`/tmp/rumoca-vehicle-608ee8b9/target/vehicle-initial-deadline-probe/`.
The original boundary probe retains binary/log hashes but not an exact
instrumentation source patch; the later initial-deadline probe retains its
patch. Neither is a production change or a closed regression.

OvervoltageProtection's bounded instrumented run reaches about `t=0.03496`
with 97,000 accepted steps, 39,758 nonlinear failures, and only 17 error-test
failures. The owner initially inferred that failure before the outer LTE
check excluded observable-induced Newton retries. The orchestrator rejected
that inference: `solve_in_place_with_observer` invokes the same callback
inside Newton, before the outer test. The next probe must classify those
inner failures; no optimization or relaxed criterion is justified yet.
Evidence is under `/tmp/rumoca-overvoltage-performance-608/target/fluid-campaign/`;
instrumented timings are diagnostic, not canonical performance claims.

The unchanged-production three-model baseline `solver-neutral-focused-b7e74df1`
uses `solver-neutral-focused-targets.json`: Vehicle still fails at the same
coordinate, while SpeedControlledDCPM and Rectifier are strict-high (two
compared, zero skipped/missing). The next candidate comparison must use this
same target file.

Vehicle's isolated compiler candidate first tried splitting guarded output
groups; that still read the old parameter snapshot and was rejected. The
revised candidate carries causal outputs through registers and orders
definitions using the existing checked discrete-real plan. Review identified
and required aggregate-read and reversed-source-order coverage. The resulting
two source tests pass through both RkLike and Bdf, and both fail semantically
on old lowering (`next=-100`, expected `100`). Existing structural cycle
rejection also passes. Record-valued when targets are rejected earlier by
Flat, so the retained aggregate regression is an array, not a record test.

The frozen six-file candidate has SHA-256
`e4dfa2b4e000c63ca2ac1d3a4d780a04cdbe2099f2a6ce12c0ffc5e5bf081747`,
with evidence in `/tmp/rumoca-vehicle-608ee8b9/target/vehicle-causal-fix/`.
It is **held, not integrated**: its final mixed-history grouping change needs
a directed check across initial algebraic projection and the post-initial
`FollowCurrentOnly` filter. The reviewer's initial claim of a demonstrated
regression was downgraded to a source-level hypothesis; alias elimination
and complete initial iteration must be checked before asserting a defect.
An earlier sampled-value objection was retracted after proving current and
pre storage disjoint. No speculative review objection is counted as a proven
runtime failure.

Overvoltage's inner observer rejection is real, but several causal claims
were rejected. A secant of the same projection is not an independent accuracy
oracle, and score `0.556` is still above the Newton threshold `0.2`, despite
being below the outer LTE threshold `1`. The first independent circuit script
also used the wrong topology: RL is parallel to CL, not in series, and the
script incorrectly clamped the diode's linearly continued branches. That
script is invalid evidence; corrected artifacts are separate.

Correct independent KCL evaluation at the recorded state gives
`CL.i=6.115124511173864e-7` and `d(CL.i)/d(CL.v)=-1.622525687329156`.
The runtime current is high by `5.52769947e-11`. Refresh block 30 accepts
the diode tear correction within its `1e-10` coordinate bound; the following
exact stage freshly computes CL.i. Dividing that current by `C=1e-7` to form
`der(CL.v)` amplifies its error to about `5.53e-4`, changing the sign of the
numerical residual used by Newton. The issue is downstream accuracy
certification, not a stale output assignment. A repair design must identify
the checked internal dependency/JVP owner and preserve the shared ME boundary;
component projection cannot recursively invoke the importer derivative handle.
No runtime patch or tolerance change has been accepted.

The valid mixed-initialization fixture is now an actionable counterexample,
independent of the Vehicle patch. The first proposed fixture was invalid:
its explicit initial equation `x=1` conflicted with `x*x=a+1` and the active
initial definition `a=pre(a)+1`. Removing that equation leaves `x(start=1,
fixed=false)` as a guess. OMC successfully initializes the corrected model
with `a=1`, `x=b=sqrt(2)`; both old and candidate Rumoca return stale `b=1`.
The valid-source and OMC/old/candidate evidence is retained under
`vehicle-causal-fix/omc-mixed-pre/` and the `mixed-pre-valid-*.log` files in
the owner's worktree. Initialization trace evidence identifies the initial
condition memory advancing before the dependent algebraic value settles.
This defect remains open; preexisting behavior is not an exception to the
zero-counterexample requirement.

Independent final review approves the six-file Vehicle causal patch at
`e4dfa2b4...` for its stated scope; the mixed-mode *regression* prediction
was disproven by identical old/new behavior. Main verified all six preimages,
frozen postimages, patch hash, and test-log hashes before integration.
Central clippy required only helper extraction, a return-type alias, and one
formatting deletion; independent review confirms those changes preserve
semantics. All 144 phase-solve unit tests and all 34 event-activation source
tests pass after the refactor, and strict all-target phase-solve clippy passes.
Canonical three-model and fixed-canary evidence remain pending. The separate
initialization counterexample is not credited to this patch.

At clean `fcb0c1db3eb446aee0907974fcc94c98dc27ff60`, the matching
`solver-neutral-focused-fcb0c1db` run compares all three targets, all
strict-high, zero skipped/missing. Vehicle recovers with 187 high channels,
zero minor/deviation channels, maximum bounded error `4.688815616e-6`, and
simulation time `1.242943965` s. Speed remains 348 high/four minor/zero
deviation; Rectifier remains 112 high/zero deviation. The official
`solver-neutral-focused-diff.{json,md}` records Vehicle entering comparison
and no losses or band regressions.

The fixed `canary-causal-fcb0c1db` compares nine models, all strict-high,
zero skipped/missing. `canary-causal-diff.{json,md}` records zero phase,
simulation, band, or coverage transitions against the previous fixed canary.
This completes Tier 1 for the causal lowering fix; it does not update the
566-model cohort measurement or close the separate initialization defect.

The projection investigation is now testing bounded final refinement under
the existing convergence contract, with no new derivative-scale policy.
The old algorithm computes and discards a converged Newton correction;
the candidate applies an accepted line-search correction and requires fresh
residual and coordinate checks before returning. Its timed diagnostic
`overvoltage-timed-diag-608` completes the actual model in `1.063111356` s.
This is diagnostic completion only; clean-patch review and canonical OMC
comparison remain pending. Earlier missing-refinement logs do not prove that
the candidate rejected the correction, and untagged high-volume diagnostics
cannot support runtime comparisons.

Independent review approves the clean projection refinement patch at
`8316640f51cf5b627cbc5fed9fffa170247d117ab3d5f52152f4d86f3297b36b`.
The orchestrator verified its frozen sources and evidence before integration.
The shared projection applies at most one final accepted Newton correction,
then requires a fresh residual and coordinate certificate. Existing scales,
tolerances, line-search rules, and numerical-plugin interfaces are unchanged.
The amplified-output regression fails on the old projection and passes after
the fix; zero-step and typed-failure cases also pass. Central validation passes
523 shared-runtime tests, 11 BDF tests, six RK45 tests, all-target strict clippy
for all three crates, and workspace formatting. Evidence is retained under
`target/fluid-campaign/torn-refinement-review/` and the adjacent
`torn-refinement-main-*.log` files. Canonical focused and canary gates are next;
the separate condition-history counterexample remains open.

Canonical Tier 1 at clean `dd3d6f4e133c7b8abe85468ad8d442375b3ac838`
confirms OvervoltageProtection recovers: one compared model, zero skipped or
missing, all 45 channels high, zero minor/deviation channels, and simulation
time `0.270294742` s. Its prior exact-model run timed out at 12 seconds and
had no comparable trace; `overvoltage-refinement-diff` correctly records a
simulation recovery without treating that earlier empty comparison as parity.
The matching three-model focus remains three strict-high, zero skipped/missing.
The fixed 20-model canary remains nine compared, all strict-high, zero
skipped/missing. `refinement-focused-diff` and `refinement-canary-diff` record
zero phase, simulation, band, or coverage transitions. All artifacts are under
`target/fluid-campaign/`; these focused results do not update the full cohort.

The valid initialization counterexample also has an ordinary-event form:
`when {initial(), time > 0.5}` must update both `a` and its algebraic consumer
`b=x`; after the later event, `a=2` and `x=b=sqrt(3)`. Review rejected the
initial-only alternative because it left `b=sqrt(2)` at that later event.
The first incorrect producer is shared event evaluation: a condition-memory
row writes its current value into live parameter storage, and the next inner
fixed-pre iteration consumes that write as its previous condition, dropping
the active edge before the algebraic consumer settles.

The generic repair follows SPEC_0007/SOLVE-C22 and SPEC_0022/SIM-001:
capture the compiler-issued condition-memory lanes once per complete outer
Appendix-B pass and overlay that immutable history during its inner solve.
Current condition rows still write live storage for the next outer pass.
Ordinary pre advancement and clock ownership retain their existing contracts;
there is no initial-only policy or solver-specific path. Independent review
approves v2 at
`a4091d07c3413e5b900160b112a25dc11ed7a16ac575842614372919526baf8e`.
Main verified all source/evidence hashes and integrated the exact postimages;
the sole preimage difference was previously reviewed fixture formatting.
The valid initial and vector regressions now pass with both BDF and RK45,
including explicit square-root values. Central validation passes all 523
shared-runtime tests, 36 event-activation source tests, and 19 boundary tests.
Strict all-target solver clippy and workspace formatting also pass.
The evidence bundle is `target/fluid-campaign/condition-snapshot-review/`;
canonical focused and fixed-canary validation remain pending.

Canonical Tier 1 at clean `ca7852150e8071144ba2606416a9bf85b0b2b3b0`
retains three focused models and nine canary models compared, all strict-high,
zero skipped/missing, and zero transitions in the official condition diffs.
The complete `tier2-shared-runtime-ca785215` run then reports 202 raw
completions, 186 compared models all strict-high, 16 tracked exclusions, and
zero missing traces. The original Speed counterexample remains closed.
The overall gate still fails coverage/runtime ratchets. Official comparison
against 608 records Overvoltage and Vehicle recovering, but RevoluteConstraint
leaves the strict-high set and excluded ParallelPumpDropOut loses execution.
The two owners must first isolate the responsible shared change and identify
the actual failing projection/sensitivity block. A tiny global worst residual
does not identify the block whose coordinate certificate failed. No additional
Fluid work or new solver-specific policy is authorized by these measurements.

The new regressions are now isolated further. Removing only the shared
condition snapshot leaves ParallelPumpDropOut's singular sensitivity failure
unchanged. Restoring that snapshot and removing only the final-refinement
delta recovers execution (`sim_ok`, `ic_ok`, `0.08079308` s). This is a
diagnostic ablation, not a repair or parity claim: its comparator did not run
because Node was unavailable. The retained result is under
`/tmp/rumoca-parallelpump-target/fluid-campaign/parallelpump-tearing-ablation/`.
The owner must next identify the first singular matrix and its physical
coordinates; no rank threshold, epsilon, or model-specific exception is
accepted from this evidence alone.

RevoluteConstraint's retained diagnostic reproduces the initialization error.
During logged certifying sweeps, every block is settled, but repeated accepted
final corrections induce changes near `1e-16` in block 1886, row 1225,
coordinate 126, invalidating earlier rows. The log covers the first 16–31
sweeps, not all 128; an exhausted logging cap is not evidence about later
sweeps. The refinement allowance currently resets on every torn-block call.
Independent direction review approves moving that allowance to one token per
issued block for the entire global projection call, consumed only on accepted
final refinement. Every residual and coordinate check must remain intact.
Implementation and a generic multisweep regression are pending; Overvoltage's
improvement must survive. The stable diagnostic artifacts are in
`/tmp/rumoca-revolute-ca785215/target/fluid-campaign/revolute-ca785215-diag/`.

Independent review approves refinement-lifetime v2 at
`830ce6ad3c3b6735af68a76b82b24472959f1c98e3b73bafe657caf5a2ddb1e8`.
Each issued algebraic block now retains its own allowance across all global
projection sweeps. A torn attempt commits consumption only after returning a
retained settled correction; declining attempts restore both coordinates and
the caller's allowance. The original inconsistent two-row fixture was rejected
and replaced by a consistent nonlinear system with a nonsingular root. That
fixture fails semantically under the old per-call allowance and passes after
the fix. Review also strengthened the refund test to check the corrected
amplified output, rather than merely successful return. V2 groups existing
arguments into contexts and adds no lint exceptions. Main verified all frozen
source and evidence hashes; 525 shared-runtime tests, strict all-target solver
clippy, and workspace formatting pass. The review bundle is
`target/fluid-campaign/refinement-token-review/`.

The development Revolute diagnostic recovers initialization and completes, but
its `12.118536906` s simulation used an outer 120-second diagnostic timeout.
It is not a canonical 12-second-budget or parity pass. Canonical validation
must still establish Revolute recovery and preserve Overvoltage's improvement.

Canonical Tier 1 at clean `6e5b695fa84407add0907e8e79a05b287fbbfd0b`
now confirms RevoluteConstraint recovery: one model compared, zero skipped or
missing, all 918 channels high, zero minor/deviation channels, and simulation
time `7.748282834` s under the unchanged 12-second budget. The exact-model
Overvoltage run remains strict-high (one compared, zero skipped/missing), with
zero transitions against its previous exact-model run. The fixed 20-model
canary remains nine compared, all strict-high, zero skipped/missing, and zero
phase/simulation/band/coverage transitions. Evidence is
`revolute-token-canonical/`, `overvoltage-token-6e5b695f/`,
`canary-token-6e5b695f/`, `overvoltage-token-diff`, and `token-canary-diff`
under `target/fluid-campaign/`. These focused checks do not update the latest
full-cohort count of 186 at ca785215.

ParallelPumpDropOut's first failing sensitivity block is exactly singular at
`t=0.4000000000000001`, with a six-by-six matrix of rank five and an exact
zero directional right-hand side. Its free direction shifts three junction
enthalpies; the other coordinates are their heat flows. Existing MLS chain
transformations do not apply to this three-way junction. An early partial
JVP shifted enthalpies without recovering dependent temperatures and therefore
did not establish a missing compiler block; that inference was rejected.

The accepted narrow repair returns the zero particular solution for an exact
zero RHS in the shared seed-block solve. It makes no claim of a unique inverse
or chart. Nonzero RHS values still use the existing LU path, and the complete
selected-JVP certificate still runs after downstream recovery. The regression
uses matching residual/JVP equations and a downstream alias requiring a
nonzero recovered direction. Central old-red selects both tests: the consistent
singular case fails, and the inconsistent case retains typed refusal. After
the three-line production change, all 527 shared-runtime tests, strict
all-target solver clippy, and workspace formatting pass. Independent review
approves the exact source and shape/finite contracts. The frozen bundle is
`target/fluid-campaign/zero-rhs-review/`; actual-model validation is pending.

Two invalid validation attempts are excluded from this evidence: an agent
actual-model build started before restoring the production change after its
ablation, and a central copied test module retained an older mtime, selecting
zero tests from a stale Cargo binary. Both were caught before claiming results.
The production bytes were frozen, test mtimes refreshed, and the central
semantic old-red/new-green checks above executed with nonzero test counts.

The zero-RHS Tier 1 validation at clean 456f1619 recovers ParallelPumpDropOut
under the unchanged canonical budget, retaining its existing pointwise
exclusion. Three focused models and nine canary traces remain strict-high,
zero skipped/missing, with zero transitions. The retained trace audit verifies
its five state temperatures within 0.0015 K of OMC and confines the five worst
junction-channel errors to [0.4, 0.6]. No strict-high claim is made for that
excluded model.

The subsequent full 566-model run has 204 completions, 188 compared, 187
strict-high, one deviation, 16 tracked exclusions, and zero missing traces.
TwoMass now executes but exposes 17 deviation channels, including initialization;
it is the highest-priority actionable discrepancy. RevoluteConstraint recovers
strict-high and ParallelPumpDropOut recovers excluded execution. IMS_Start now
times out, while seven baseline-certified models remain absent. The complete
quality gate fails. Both implementation agents are assigned read-only TwoMass
triage: one maps every divergent channel and its time support to model
semantics, and the other challenges any proposed oracle-boundary explanation.
No exclusion, tolerance, or production change is authorized by the matching
model-family name alone. Evidence is `tier2-shared-runtime-456f1619/` and
`tier2-zero-rhs-diff.{json,md}` under `target/fluid-campaign/`.

The actual-model convergence diagnostic in
`/tmp/rumoca-fluid-speed-counterexample-review/convergence-evidence.md` now
compares tightened Rumoca and OMC runs using the unchanged production
comparator. Canonical Rumoca versus the original OMC reference has 348 high
channels and four deviations. Tightened Rumoca versus that same original
reference has 348 high channels, four minor channels, and zero deviations:
this is **model-level strict-high** under
`classify_trace_metric_channel_distribution`. The orchestrator rejected an
agent's contrary conclusion, which incorrectly required every channel to be
high. Tightened Rumoca versus tightened OMC has all 352 channels high. These
are diagnostic runs, not a tolerance-based repair or canonical gate closure.
The original reference therefore does not prevent a runtime accuracy fix;
the work remains on the runtime at unchanged canonical tolerances.

An isolated actual-model probe also checked 338 Jacobian-vector products
against finite differences, agreeing to approximately `6.3e-10` relative on
substantive components. Its evidence is
`/tmp/rumoca-speed-runtime-67978-clean/target/speed-jvp-diagnostic.tsv`.
No derivative or algebraic-publication defect was demonstrated. Endpoint
comparisons against linearly interpolated reference output contain reference
interpolation error and cannot, by themselves, locate an integration defect.
No production runtime patch has been accepted, and the original counterexample
remains open.

The full cohort at clean `67978a5d263e726ddbfb596e4735f7b55d6a2e7c`
reopens campaign validation: SpeedControlledDCPM is near with four deviation
channels, and baseline-certified PrismaticConstraint timed out in Solve.
The run has 193 compared traces, 192 strict-high, one near, 22 skipped, and
zero missing traces. Evidence is `target/fluid-campaign/tier2-67978a5d/`.
Per SPEC_0033 §6a, unrelated capability work is paused until the actionable
counterexample count is zero. Leibniz/Confucius investigate runtime semantics,
Pascal/Linnaeus investigate comparison and trace publication, Euclid investigates
the timeout, and Locke investigates the historical NandGate simulation loss.
Euclid and Locke review each other's proposed fixes. These are
investigation assignments, not conclusions about the failing layer.

Independent raw-sample checks falsify the initial interpretation that OMC's
saturation channel is identically zero: both traces contain a 112-sample pulse
near 19.617. The report's zero reference range comes from its p05/p95 policy.
Both flagged subtraction expressions satisfy their source equations exactly
at Rumoca's published samples. Artifact paths and digests are retained in
`target/fluid-campaign/speedcontrolled-independent-raw-review.json`.
These checks do not close the counterexample: the power-feedback pair still
differs, and changing normalization policy is not an accepted repair.

Pascal's separate grid and event-coordinate analysis is retained in
`/tmp/rumoca-fluid-speedcontrolled/speedcontrolled-comparator-review.{json,md}`
with `raw_grid_analysis.py`. Confucius independently confirms the source
identities and rejects treating the heuristic shape label as proof of a time
shift. A central `cargo xtask repo msl -- debug-model` run now retains exact
Flat, DAE, structural DAE, and Solve JSON in
`target/fluid-campaign/speed-ir-67978-diagnostic/`; its manifest records source
digests and differences from canonical settings. Its times and all values of
`addSat.y`, power `feedback.y`, and `powerController.y` exactly match the original
Tier 2 trace. It supplies reproducible IR evidence, not a new parity verdict.

Linnaeus independently approves Pascal's completed grid/event-coordinate
analysis as diagnostic evidence. Confucius's subsequent IR review is retained
in `/tmp/rumoca-fluid-speed-counterexample-review/runtime-probe-review.{md,json}`.
The motor current state already differs at the first post-step output, and the
retained Solve IR has no manifold-projection blocks. Neither finding proves
the numerical cause. The next probe must compare accepted BDF states and
interior samples independently against an analytic stiff tracking solution;
endpoint interpolation equality proves consistency, not accuracy. A sustained
nonlinear forcing case is required to challenge a reducer that becomes affine.

Euclid's actual-Solve PrismaticConstraint diagnostic at source `67978a5d`
measures 12.320685 seconds in program lowering and 0.040537 seconds in runtime
vector construction. Its 60-second diagnostic budget does not close the
canonical 10-second failure. Evidence and artifact digests are retained in
`/tmp/rumoca-fluid-incompressible/target/prismatic-67978-solve-diagnostic-analysis.txt`.
The next temporary timing probe separates `state_selection::prepare`,
`lower_selection`, and `lower_solve_artifacts`; Linnaeus independently reviews
that direction. No internal subroutine is blamed before that measurement.

The three-call diagnostic then measures 8.025462 seconds in state-selection
preparation, 3.840957 seconds in `lower_selection`, and 0.025870 seconds in
artifact lowering. Their sum matches the worker timing within 95 microseconds;
the Solve IR hash is unchanged. Evidence is
`/tmp/rumoca-fluid-incompressible/target/prismatic-three-timer-settings-and-result.txt`.
The probe was restored; the orchestrator's tracked-source comparison finds no
compiler differences from `67978a5d` in that workspace. Repeated-work or cache
fixes remain unproven until the dominant preparation work is measured internally.

The NandGate attribution review is closed independently by Linnaeus. Clean
parent `00ace1da` reproduces the same `EX001` at `1e-13`, with identical readable
Flat and structural DAE artifacts. The report, exact commands, and comparison
normalization are in `/tmp/rumoca-nand-target-00ace1da/diagnostics/nandgate-review.md`.
The failure predates this campaign; it is not repaired or hidden. A dirty worker
tolerance change is only an unproven explanation for the historical success.

The orchestrator held the first runtime-probe build after finding 27 tracked
file differences between its workspace and the measured source, including
compiler files unrelated to instrumentation. The mismatch inventory is
`target/fluid-campaign/runtime-probe-source-mismatch.json`. Before using an
actual-model probe result, its owner must preserve those paused candidates and
apply only the declared instrumentation to an exact `67978a5d` snapshot.
The standalone analytic reducer and the actual MSL model are separate
experiments; the reducer's analytic solution cannot judge the motor's states.

The corrected runtime probe uses `/tmp/rumoca-speed-runtime-67978-clean`.
The orchestrator verified exactly two declared instrumentation files differ
from the measured source, including a comparison of vendored dependencies.
All published actual-model trace values match Tier 2 exactly. Its manifest is
`target/speed-runtime-probe-manifest.json` in that isolated snapshot.

Review held two insufficient reducer variants and corrected the mistaken claim
that `atol=1e-6` matches the canonical worker. The final standalone experiment
uses the specified sine forcing through `T=0.01`, direct accepted states,
interpolated interior values, and effective worker tolerances `rtol=1e-6`,
`atol=1e-10`. It takes 288 steps and measures maximum endpoint/interior errors
of approximately `4.24e-7` / `1.25e-6`. This is diagnostic evidence, not proof
of a solver defect or an explanation of the actual MSL discrepancy. No
permanent runtime fix, tolerance change, or counterexample closure is accepted.

Before this pause, the initial-algorithm replay candidate was held: its
checked iteration count does not prove that adding the step after the last
iteration cannot overflow (a singleton range ending at the maximum integer
is a counterexample). Its per-axis cap also fails to bound nested replay.
A revision must establish the checked initialization owner required by
SPEC_0007 and address compactness and dependency semantics. The vectorization
candidate remains held for exact redeclaration closedness, legal source
fixtures, and old-red/new-green evidence with accepted alias behavior intact.

| Workstream | Independent challenge owner | Direction and outstanding evidence |
|---|---|---|
| Record-return binding projection | Function references | Constructor-only projection accepted: two old-red/new-green regressions, 248 instantiate tests, all three actual ET002 failures cleared, unchanged fixed canary. Terminal-signature candidate rejected because real failures were unchanged. |
| Function references | Record-return binding projection | Alias exposure and partial-function selection fixes accepted. All twelve EF025 cases cleared; fixed canary unchanged. Receiver assertions now follow exact callable instance to its body. Next failures involve constructor metadata and one unresolved generic function. |
| Incompressible record selection | Enclosing record members | Exact extends-modifier selection passes 648 unit and eight source tests; actual missing T cleared. Canonical Flatten timeout remains a failure. A 60-second diagnostic reaches EF025 in 38.54 seconds; fixed canary unchanged. |
| Enclosing record members | Incompressible record selection | Revision 2 accepted after scoped package, missing-location, and structured-path counterexamples were addressed. Central 256 instantiate and ten record source tests pass; all seven actual molarMass errors cleared; fixed canary unchanged. |
| Wall-friction members | Selected medium constants | Inherited defaults collected at the upstream producer. Independent Locke review, old-red/new-green and 249 instantiate passes; all 14 actual use_mu failures cleared, fixed canary unchanged. Component-map and resolver-fallback directions rejected. |
| Selected medium constants | Wall-friction members | Rendered-name owner bridge rejected. Typed-owner v3 also rejected and restored: both source tests still collapse separate owners into callable instance zero. Trace the first actual owner loss before another revision; same-owner separate package slots and transitive dependencies remain adversarial cases. |

The strengthened binding-source fixture gives the modified Holder and lexical
root different class occurrence identities, and its 140-test suite passes.
However, deliberately restoring the old owner-class pairing also leaves both
new scope tests green (`/tmp/rumoca-fluid-scope-old-pairing.log`). The fixture
does not yet distinguish the defect: declaration-global record types are
insufficient to reproduce the instance-dependent failure. Its regression
claim is rejected pending a fixture with actual effective occurrence types.
The correct production pairing was restored immediately after this check.

The replacement fixture instantiates WaterMedium and OilMedium through the
same Holder declaration with different effective record types. With the
correct pairing, both the valid model and the cross-medium mismatch check
pass. Restoring the old pairing makes the valid model fail with ET002
(selected record versus generic PartialMedium record) and ET001 for the
selected T/h members; the negative remains rejected. Logs are
`/tmp/rumoca-fluid-scope-occurrence-fixture.log` and
`/tmp/rumoca-fluid-scope-occurrence-old.log`. Correct production code is
restored, and independent review of the replacement fixture is pending.

Locke and Linnaeus independently accepted that replacement fixture and its
before/after evidence. The orchestrator accepts it as regression evidence for
the binding-source capability only. It establishes neither callable-constant
specialization nor closure of the wall-friction failure.

Locke's earlier claim that two distinct Pipe declarations necessarily collide
in a DefId-keyed component map was retracted after challenge: those declarations
have distinct DefIds, and no legal differing per-element selection reproducer
was supplied. The component-map candidate remains rejected for the separate,
proven reason that the real failing root is a class identity. Reviewer claims
require evidence too.

Linnaeus independently held the selected-medium constant candidate, normalized
diff digest `27b59118144d8855fc44b105181c5f516f39f036dc66793fdeab48f70099e2ac`.
Besides the rendered-name owner bridge, AST requests discard owner identity,
and the test does not prove each call site selects the correct callable instance.
Revision must preserve typed occurrence ownership and strengthen that assertion;
the changed diff requires a new review.

No direction approval in this table is approval of an unvalidated patch.

The two-file callable-signature candidate passed all 140 typecheck tests but
left the actual Fluid census unchanged at
`target/fluid-campaign/signature-candidate/`: three ET002 at the same source
positions, 16 EF024, three EF019, and one ED019. It is rejected as a repair for
the reported Fluid errors, and the original sources have been restored. Its
pre-candidate source and exact before/after digests remain under
`target/fluid-campaign/signature-candidate-source/`. A plausible identity
lookup issue was not sufficient evidence of the actual diagnostic's cause.

The enclosing-record candidate received independent Euclid review, but main
validation reopened it: 247 of 248 instantiate tests passed, with the legal
`selected_package_record_member_uses_inherited_redeclared_record` source failing.
Its seven focused callable-record tests still passed. The new nested-selection
guard does not yet preserve the exact redeclaration carried by an extends
modifier. All ten candidate files were restored from saved originals; the
manifest and digests are in
`target/fluid-campaign/enclosing-record-candidate-source/`. Approval did not
override the failing validation gate.

Leibniz independently approved the four-file alias-exposure producer candidate.
That exact digest-checked candidate is now under main validation. No compilation
gain is claimed before the canonical measurement finishes.

The alias candidate subsequently passed its fixed canary with no transitions;
the canonical Fluid run clears three EF019 diagnostics to EF025 without a
compilation gain. Its manifest records acceptance and the additional independently
reviewed test assertion separating exposed slot from selected implementation.

The record-projection candidate now has two demonstrated old-red/new-green
tests and 248 passing main instantiate tests. Confucius accepted its narrow
direction after retracting an unsupported homonym objection: the cited legacy
helper returns on exact identity before its textual branch. The existing fixture
repair preserves its assertions while giving Pkg.Inner/Pkg.Outer their canonical
nested AST layout. Canonical Fluid validation is running; no gain is assumed.

Temporary probes for WallFriction and EF025 were removed after their diagnostic
runs. Evidence remains in `/tmp/rumoca-fluid-use-mu-scope-main.log` and
`/tmp/rumoca-fluid-ef025-main.log` respectively.

The partial-function alias guard received independent Leibniz approval for
validation at production digest `041d83571323ef51632cd8fec684e4fffa36938a96deb07026bde376fdbca14d`.
The ordinary AST fixture was repaired to preserve valid nested package identity;
its old run fails and its new run passes. The orchestrator confirms the change
uses the partial property, not a Modelica name exception, and preserves the
partial declaration for downstream strict checking rather than borrowing an
icon's implementation. Main validation passes 649 flatten and 249 instantiate
tests and all-target clippy. Canonical measurement is pending; the exact
sources are recorded in `partial-function-selection-candidate-source/`.

Test-only fixture helper refactors for the accepted record-projection and
extends-member patches preserve their assertions and production code. The
same main validation closes their earlier clippy failures.

The canonical partial-function run clears all 12 EF025 cases, advancing Flatten
completion from 1/23 to 13/23. DAE compilation and simulation remain zero.
The broader flatten source suite exposes two receiver-redeclaration assertion
failures in `receiver_function_redeclare.rs`. Earlier exposure-preservation
work may have changed the asserted identity contract, but that is only a
hypothesis: acceptance requires proving each call selects its correct concrete
implementation and retaining the sibling/qualified-receiver isolation checks.
Leibniz owns that investigation; no assertion is waived. The other 57 source
tests reached before this failing binary pass. New canary validation is running.

The partial-function fixed canary completes successfully with nine compared
traces, all high, zero skipped/missing, and no simulation or band transitions.
Evidence is `canary-partial-function-selection-diff.{json,md}`.

The orchestrator reopened the typed-owner revision before integration: its
12-file patch omits callers of the changed `resolve_source_constant` API,
removes semantic identity from reachability retention, and does not yet prove
that the name-keyed callable table retains separate instances. The first is a
concrete incomplete handoff; the latter two require code or source-test proof.
Locke must supply a complete exact patch and passing same-declaration owner
fixture. No candidate files have been applied to main.

Receiver assertion review is closed. The default alias legitimately retains
the exposed slot DefId; the revised four source tests follow the call's exact
FunctionInstanceId to its collected body and prove the expected Double/Triple
implementation, while retaining sibling and qualified-receiver isolation.
All four pass. Both existing receiver simulation tests pass, and the complete
703-test core suite passes. The exact fixture and validation paths are in
`target/fluid-campaign/receiver-assertion-review/manifest.json`.

Independent Euclid review confirms that the enclosing-record candidate must
not let failed diagnostic span conversion discard a rejected member decision.
It also requires a same-scope two-package/shared-original-slot case before
accepting global alias updates. The structured-path key objection is resolved
by QualifiedName keys in the revised candidate; remaining objections stay open.

Euclid independently approved enclosing-record revision 2 at review request
digest `808923d73fb4c565a752531d11aa84523cc037a984038b254a2eaf13c39c9021`
and diff `605e498f0723a4c49da30e869463a0becf58870493c8dfd0ebb0339a7f38d8c3`.
The revised source propagates missing diagnostic context as a typed error,
uses structured path keys, and scopes selected member decisions by package.
The negative and paired-package tests close the specific review objections.
The orchestrator applied the frozen 14 files after verifying main preimages;
central validation passes 256 instantiate tests, ten record-identity source
tests, and clippy. The actual Fluid and fixed-canary gates remain pending.

The actual enclosing-record revision 2 run clears all seven molarMass errors,
advancing Flatten completion to 20/23. DAE compilation and simulation remain
zero; only three canonical timeouts remain in Flatten. The orchestrator had
challenged whether Instantiate could affect the presumed callable-body path;
the unchanged-failure prediction is disproved by the measured run. No extra
Flatten repair is justified by that hypothesis. The exact source-to-error path
should still be explained, without describing the overlay fixture as a test
of a directly converted callable body. The fixed canary is running.

The enclosing-record revision 2 fixed canary completes with nine compared
traces, all high, zero skipped/missing, and no changed simulation or agreement
bands. The Tier 1 gate is complete; see `canary-record-index-rev2-diff.{json,md}`.

The typed-owner v3 source fixture fails before production changes: both calls
from different owners resolve to FunctionInstanceId(0), and selected constants
are absent. This is concrete evidence that occurrence ownership is collapsed
at callable collection, independent of the later arithmetic error. The v3
patch now includes its API call-site closure; final peer review remains pending.

Typed-owner v3 is rejected and all 16 files restored from verified preimages.
After fixing only private-field access and a test-only wrapper compile issue,
both source regressions still fail identically: the two owners share callable
instance zero and their selected constants are absent. A plausible allocation
registry was insufficient; the author must identify the first point where the
actual call loses owner identity. No broad run was made with this candidate.
Exact tested sources and logs are retained in `typedowner-v3-source/`.
