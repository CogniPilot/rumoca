# Shared affine elimination layout experiment (held)

The prototype is retained at `d0f1501a` on
`experiment/shared-affine-layout-d0f1501a`. Its code was removed from integration
after the focused gate failed and no performance benefit was demonstrated.
It was never published in PR356. The source argument below is not a promotion
of the experiment.

The be97 Fourbar interior Sim profile contains 940 CPU-clock samples. Of 73
`memcmp` leaf samples, 19 unwind through `AffineEliminationLayout` equality,
seven through structural-pattern equality, and 47 have no recovered caller.
The raw profile is `/tmp/rumoca-fourbar-projection-repeat-perf-be97/stacks-sim.txt`;
the compact count is `target/fluid-campaign/fourbar1-study/layout-comparison-attribution-be97.json`.
These counts identify work, not a guaranteed removable runtime fraction.

`TornNewtonCache::solve_scaled` compares its saved `TornReads.layout` with the
issued layout. `CheckedPivots::bind` independently compares `TornSystem.layout`.
Both saved layouts originate from `layout.clone()`. The previous representation
deep-copied immutable arrays and compared their contents on subsequent solves.

The candidate changes only the IR representation: a private `Arc` holds the
same checked, immutable fields. Clones share that payload. Equality accepts
pointer identity or the complete original structural comparison, so independently
derived equal layouts still compare equal. A different causal order still
compares unequal even when the pattern is unchanged. No payload mutation or
unchecked constructor is exposed. This follows SPEC_0029 §3 data-integrity
ownership and SPEC_0007's checked Solve boundary; it changes no Modelica rule.

Runtime source, matrix values, scaling, exact-zero guards, pivot checks,
factor updates, recovery arithmetic, and final residual checks are unchanged.
This is storage sharing, not reuse of a numerical result at another point.
The two pattern accessors cease to be `const`; there are no repository const
callers. The layout has no serialized representation.

Independent source review accepted the two-file change. The focused regression
covers shared clones, equal independent constructions, and unequal causal
orders. All 345 IR and 586 solver unit tests pass, as do strict all-target,
all-feature clippy for both crates and workspace formatting. Logs are under
`target/fluid-campaign/shared-layout-{ir-tests,solver-tests,clippy,fmt}.log`.
The [Tier1 audit](shared-layout-tier1-d0f1501a-audit.md) records a failed focus
gate and passing canary. Thyristor loses completion at its12s deadline; all
13 surviving focus/canary traces are byte-identical. Fourbar is observed at
6.828s versus ce1's5.978s. Host load rose from2.65 to18.52 during the candidate
focused run; neither timing causality nor a speed improvement is established.
No retry or full-cohort run was performed for this experiment.
