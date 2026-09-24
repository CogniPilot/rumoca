# Fourbar performance: current resume point

Goal: beat OMC for both Fourbar simulation and model preparation. Not achieved.
Use general compiler/shared Model Exchange improvements, retain numerical
certificates and exact model identities. Fluid breadth remains paused.
Route through AGENTS.md/specs and keep delegated reads/tasks bounded.

## Source and publication

Integration: `/tmp/rumoca-fluid`, branch `fluid-library-coverage`.
Validated production: `ce1a85bc9785e3ba9eda10249d1c8316ea8a472d`;
test-only CI fixture follow-up `e1b022140` also passes workspace clippy/fmt.
Recovery-support optimization remains removed; its isolated prototype is
`985c5f4b6` on `fourbar-recovery-support`. No material timing gain was proved.

Draft [PR #356](https://github.com/CogniPilot/rumoca/pull/356) is published at
`1ddec2086c3a3c7a7136d04a205f81e1773e0e74`, exact tree of `2b8a17dd3`.
Modifier repairs are published; Identity-affinity and the fixture fix await
the next single-commit update. Local PR checkout also has unpublished test
commit `a8ff60d1`; do not drop it when syncing/squashing. Preserve one squashed James Goppert
commit, Signed-off-by, no AI coauthor; recheck remote then explicit force-lease.
Do not edit/build/reset `/home/jgoppert/git/rumoca`, which has unrelated work.
Only its user-authorized `dev/fourbar-current-handoff.md` is ours to update.

## Correctness evidence

[Modifier provenance repair](selected-member-provenance.md): preserve the
substituted value's occurrence independently of the written source, including
array planning/replay and family reindexing. Final source review approved;
262 instantiate and 67 array contracts pass; eval-ast109 passed before the
last array-context-only follow-up; focused clippy/fmt pass.

[be97 Tier1](fourbar-be97aace-tier1-audit.md): focus five raw completions,
three compared/high, two exclusions, zero missing; canary nine compared/high,
zero exclusions/missing. Both partial gates pass; all14 traces match513.

[be97 full566](provenance-full-be97aace-audit.md): 307 compiled, 288 simulation
attempts, 232 IC successes, exact211 raw and192 compared/high identities,
19 exclusions, zero missing. All211 Rumoca/OMC trace files match513 byte-for-byte;
all compared channel scores and initial metrics match. This restores the two
controlled-DC high models and Thyristor completion lost at7143. Four batteries
advance to later failure phases, without becoming simulation successes.
The full gate still fails historical accounting/runtime floors and the same
four high-model12s timeouts: IMC_YD, SMPM_VoltageSource, SMR_DOL,
UniversalConstraint. No baseline promotion or claim of all-history preservation.

## User's current priority: perf and generated equation comparison

[OMC equations and fresh profile](omc-fourbar-runtime-math-and-capture.md):
OMC C uses one12-variable nonlinear closure, then linear2/16/20 systems,
with generated analytic Jacobian callbacks. Separate initialization systems
must not be counted as runtime duplicates. The fresh retained-binary profile
completed with298 CPU samples/no lost; itsCSV exactly matches the retainedCSV.
It covers the whole process, including initialization andCSV. It observes
LAPACK `dgesv` and analytic nonlinear Jacobian callbacks. Do not infer a
sparse-solver advantage or add overlapping inclusive percentages.

Rumoca's timed path is Cranelift-generated kernels plus shared Rust projection,
not the C-export template. Historical affineowner1531 has616coordinates,
16tears; exact correspondence to OMC systems requires semantic variable mapping,
not dimension matching. Repeated observable projections dominated the prior
instrumented census (~62% inclusive); no certificate-safe reuse is yet proved.
The actual/trial error norm and all guards must remain.

Fresh cleanbe97 capture: `/tmp/rumoca-fourbar-projection-repeat-perf-be97/`.
One run completed exit0, 6.366s instrumented simulation, interior Sim window
216113–216118 with940 CPU-clock samples/no lost. Kepler is finishing attribution,
trace verification and equation mapping. Source checkout remains cleanbe97 at
`/tmp/rumoca-fourbar-projection-repeat`; no repeat-cache probe was introduced.
OMC profile is whole-process/unpinned; this Rumoca profile is Sim-only/core-bound.
Do not call those matched timing scopes. Canonical be97 Fourbar:6.239s focus,
6.714s full versus retainedOMC0.335s; output/error-control workloads differ.

## New general compiler improvement

[Identity-affinity proof](identity-affinity-proof.md): typed Identity has no
inputs, but its missing interaction rule discarded a pure-call affinity proof.
One production arm now proves Fourbar's362-row velocity block affine; its
position block stays nonlinear.344IR tests and independent review pass.
[ce1 full audit](identity-affinity-full-ce1a85bc-audit.md): exact211raw/192high,
19excluded/0missing preserved;208traces identical, onlyFourbar1/PlanarFourbar/
Engine1b change within high bands. Same historical floors/fourtimeouts remain.
ObservedFourbar5.978s focus/6.588s full; not a demonstrated large speedup.

PR1ddec run35945083353 exposed missingbinding_value_scope in a flatten test
initializer (E0063). Test-onlye1b022140 fixes it;51contexttests, workspace
all-target/all-feature strictclippy andfmt pass. No main build is live.

## Coordination and next actions

Small live state: `target/fluid-campaign/coordination.json`. All main runs
58845(full),13002(checks),64214(transition) are terminal. Canonical transition
reports zero model/cohort/band changes againstbe97.

Kepler `01a0d0da-7ea4-7d63-b032-8e1be1f1a16e` prepared an unbuilt/unrun temporary
JIT-symbol-to-equation mapping probe at `/tmp/rumoca-fourbar-schedule-owner-ce1/`.
It remains dirty in `/tmp/rumoca-fourbar-projection-repeat` atop cleancommit9d7a15a8.
Peirce `01a0cffd-6c59-7cc2-8804-3bea2f24ef79` is reviewing its source binding,
multiple-owner matches and compile-only logging. Do not run until that review
finishes and the lane is released; restore both probe checkouts after capture.

Next: publish validated Identity/test repairs as one signed-off draft commit;
run one approved source-bound owner capture; use exact equation attribution
for the next material compiler improvement. No broad executor/AD/cache rewrite
without evidence. Current OMC profiles and Rumoca profiles have different
scopes; preparation victory is unproved. All commits James Goppert
<james.goppert@gmail.com>, `-s`, no AI coauthor. No merge or baseline promotion.
