# Closed conditional literal-plan consumption

Source `84459f6e4` reproduced a checked whole-closed-value replacement being lost during arm replay: 0 passed, 1 failed at the consumer assertion. Source `2e092cb428f9c33623a2815e408c196171f769a1` passed the four model-conditional controls, all 267 structural tests, and scoped all-target Clippy. Four is a subset of 267, not an additional count.

The replay closes the original checked conditional before consuming its existing checked literal plan in the restored parent scope. Issued guard expressions retain their original operations and identity. The checked close carries its actual provenance; no new panic or constructor/wire admission was introduced. This follows SPEC_0040 STRUCT-T10 and SPEC_0029's single evaluation owner.

The unchanged seven-model MSL bracket completed under canonical model phase 10 s, solver 12 s, simulation watchdog 14 s and source startup 60 s. All seven still failed structural matching: Cascade Continuous 51/53; its three clock variants 44/46; ToroidalCore Airgap and QuadraticCrossSection 77/79; SMPM_NoLoad 391/404. Thus this construction fix does not close those singularity regressions. No simulation or OMC parity is claimed for this bracket.

`receipts.json` binds the raw logs, guardians, original requests, helper, artifacts and individual model results retained under `$HOME/scratch/rumoca/pr427-structural-diagnostics-86c-20261012`. The same guardian limits apply: 600 s, aggregate resident plus swap 4 GiB, host reserve 16 GiB, owned target cap 8 GiB and scratch reserve 8 GiB. Full combined-head validation remains pending.
