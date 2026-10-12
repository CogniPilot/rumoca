The checked event-clock producer now reads `PreDiscreteValue` for `Clock(condition)`.
The previous producer used Current, which could admit a tick from a transient
ordinary fixed-pre settling pass. The change reuses existing DAE pre coordinates,
Solve pre storage and fixed pre mode; it adds no runtime interpretation or public API.

The governing rule is MLS 3.7 operator 16.4: the clock ticks on `edge(pre(condition))`,
one event iteration after the settled condition rises. The immutable MLS tag commit
is `b7361fab8cad41971d2fe55e329f09c951d3a090`; the retained synchronous chapter has SHA256
`e1daafc61840e45f9aba9708e77b09e24623c48c88477fd024ac14302ce20d5d`.
SPEC0022, SPEC0033 and SPEC0050 govern the source owner and evidence separation.

The minimal signed-off source chain is `77ec1b28c`, `5cc9d3219`, `2a2d8d405`.
The final exact source is `2a2d8d405f2590305616fba12e3000a72ec0aaae`.
The temporary diagnostic commit `7e130ef3f` is excluded from this chain.

The feature-gated minimal conjunction regression failed BEFORE: the second
initialized input latch was 0 rather than 1 at 0.02 seconds. A temporary trace then
showed Current conjunction P83 rising during inner settling while Pre P189 stayed 0,
admitting reset and leaving its held output 1. All 170 snapshots kept Pre P189 at 0.
The independent unticked Hold control passed, excluding a general Hold-start defect.

At final source, the two focused conjunction/no-tick tests and all 13 existing
event-clock/initial-tick/held-initialization controls passed. Scoped phase-DAE and
suite-core Clippy plus workspace fmt passed. An initial nextest-list wrapper error
ran zero tests and remains retained separately; the corrected wrapper selected 13.
All successful stages have equal tracked-source pre/post manifests and terminal
birth-verified cleanup with no live owned descendants.

The canonical fixed20 canary exited 0 with the unchanged roster and 10/12/14/60-second
phase/solver/parent/startup budgets. Its 20 stage/error/comparison outcomes match b077:
19 flattened, 17 DAE/Solve-balanced, 15 simulations completed, 14 compared and high,
one policy-excepted, zero missing, zero deviation. This is focused evidence.

The unchanged original LogicalSample completed its full 2-second, 500-interval
simulation. Its first conjunction sample is 3.000000000000333 at
0.0484933420103447 seconds; the hand-computable target is 3 at
asin(0.3)/(2*pi) = 0.04849334201033915 seconds. The initialized latch remains true
and reset remains false before that tick. Its strict command exited 1 because the
current OMC reference was ineligible: zero compared, one missing, no parity credit.
Failed-status reference CSVs are diagnostic data only. Full-cohort eligible OMC
comparison and combined broad validation remain separate required work.

`canary-delta.json` retains all20 outcomes and exact raw hashes; `original-logical-sample.json`
binds the original trace and analytic oracle. `revised-artifacts.json` shows the three
DAE condition nodes changed Current to Pre and reset guards load P189 instead of P83.
The sealed CLI's actual build-info is `2a2d8d405f25`; worker/test ELF bindings are in
the external `sealed-after/executables.json`. Raw evidence remains immutable under
`$HOME/scratch/rumoca/pr427-logical-sample-first-tick-b077-20261012`.

The committed canary ledger stores exactly equal before/after outcomes once,
retaining all 20 identities and both raw artifact digests. Its original full
receipt digest is recorded; no raw artifact was normalized.
