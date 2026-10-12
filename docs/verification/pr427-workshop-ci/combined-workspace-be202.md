# Combined workspace checkpoint at be202

The actual nextest run at `be20244f409d7c8e984328dc9cb2953db8ffdae9`
completed **10,312 tests: 10,288 passed, 24 failed, zero skipped**.
Nextest took 236.774 seconds; the complete build/test guardian took 473.600
seconds and peaked at 4,612,396 KiB aggregate resident RSS plus swap.
There was no resource stop, and tracked source matched before and after.
This is a failing checkpoint, not final-tip acceptance.

Fifteen failures report an unavailable mandatory MSL fixture: four suite-core
library-call tests and eleven MSL simulation tests. The frozen checkout had
not provisioned its ignored `target/msl` directory. They remain failures in
the census; they do not establish numerical regressions or passes.

The other nine failures are the 2,027-line GALEC test module, 274 Solve totality
assertions against the unchanged ceiling of 272, two mixed initial/periodic
elsewhen tests, two continuation and two Frame/Dim extrema cases, and one
selected-row/window function-call interface case. Separately qualified
extrema fixes cover the four continuation/Frame cases, but that focused
qualification does not rewrite this checkpoint result.

Two preceding 600-second build attempts expired before any test execution;
both are retained. A subsequent focused run at `5b4982a62` ran all 314
phase-Solve/GALEC library tests: 313 passed and one failed because the diamond
DAG test still required the old scalar fallback despite its now checked
extrema call owner. Its original model, numerical oracle and complexity
ceilings are preserved in the proposed owner-aware census correction.
The clock/architecture command following that failed block did not execute.

The architecture repair passes already matched operation fields to projection
helpers instead of rechecking variants with three `unreachable!` branches.
The test-module split uses ordinary submodules by concern. The clock repair
derives a periodic task schedule across every branch of an ordinary target;
strict declared synchronous ownership remains separate. Acceptance of those
repairs and the corrected DAG census is pending the next actual run.

[The receipt](combined-workspace-be202.json) binds raw logs, source manifests,
recipes and guardian results. Raw evidence remains under
`$HOME/scratch/rumoca/workshop427-final-broad-20261012/evidence/`.
