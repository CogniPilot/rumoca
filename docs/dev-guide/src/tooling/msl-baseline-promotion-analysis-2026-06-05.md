# MSL Baseline Promotion Analysis, 2026-06-05

This note compares the recent full MSL quality artifacts from PRs #191, #192,
#198, and #199 against the committed baseline. PR #202 is excluded from the
promotion decision because its MSL job was cancelled before producing
`msl_quality_current.json`; the uploaded artifact only contains the placeholder
PR comment.

## Quality Snapshot

All four comparable PR artifacts are full-scope MSL 4.1.0 runs using
OpenModelica 1.26.7 and report the same quality counts. That repeated result is
strong enough to promote the quality baseline from a successful full run.

| Metric | Baseline | Recent full runs | Delta |
|---|--:|--:|--:|
| Parse | 566 | 566 | 0 |
| Flatten | 556 | 562 | +6 |
| DAE / compiled | 414 | 477 | +63 |
| IR-Solve | 336 | 387 | +51 |
| Balanced | 401 | 464 | +63 |
| Initial balanced | 401 | 464 | +63 |
| Initial-condition solve OK | 182 | 224 | +42 |
| Simulation OK | 132 | 153 | +21 |

The trace snapshot also improves on the gate-relevant model counts:

| Trace metric | Baseline | Recent full runs | Delta |
|---|--:|--:|--:|
| Models compared | 123 | 143 | +20 |
| High agreement | 65 | 89 | +24 |
| Minor agreement | 16 | 20 | +4 |
| Deviation agreement | 42 | 34 | -8 |
| Models with any bad channel | 46 | 44 | -2 |
| Bad channels | 1047 | 678 | -369 |

The severe-channel total rises from 35 to 151 because the current run compares
20 more models and 1363 more channels. The model-level gate signal is still
better: acceptable trace models increase from 81 to 109, and models without a
severe channel increase from 117 to 131.

## Speed Snapshot

The rendered PR comments include comparable aggregate speed tables for 108
trace-agreeing models:

| PR | Total throughput | Total median | Compile throughput | Compile median | Sim throughput | Sim median |
|---|--:|--:|--:|--:|--:|--:|
| #191 | 3.49 | 5.96 | 5.80 | 6.82 | 0.18 | 1.22 |
| #192 | 3.25 | 5.51 | 5.05 | 5.96 | 0.19 | 1.57 |
| #198 | 3.17 | 5.30 | 4.80 | 5.64 | 0.19 | 1.65 |
| #199 | 3.31 | 5.58 | 5.02 | 5.74 | 0.19 | 1.47 |

These values support a stable median-based speed gate more than a mean-based
gate. Mean speedup is more sensitive to outlier models and tiny OMC simulation
times, while the existing spec already defines a 20% median tolerance for system
and wall runtime ratios.

The 2026-06-05 artifacts cannot directly promote runtime speed baselines because
the generated `msl_quality_current.json` snapshots did not serialize
`runtime_ratio_stats`. The gate now preserves those stats from the OMC parity
input and checks both system and wall medians once a promoted baseline includes
them. A follow-up full MSL run after that change can safely promote the speed
baseline from the machine-readable snapshot.

## Recommendation

Promote the MSL quality baseline from the successful #198 full MSL artifact.
Do not synthesize speed baseline values from rendered markdown. Instead, use the
next full MSL snapshot generated after runtime ratio stats are serialized into
`msl_quality_current.json` to promote speed medians through the normal baseline
promotion command.
