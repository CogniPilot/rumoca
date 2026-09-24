# Causal recovery support experiment

The prototype remains on `fourbar-recovery-support` at `985c5f4b6`. It was
removed from integration after the first canonical focused run failed to
demonstrate a material timing benefit; this is not a finding of numerical error.

Integration source `24dae4bdb` contains the reviewed recovery optimization from
worker `985c5f4b6`. The shared Model Exchange projection kernel retains dense
recovery storage and the existing reduced solve, guards and certificates.

`AffineEliminationLayout::recovery_column_support` derives tear reachability
from the already-issued causal partition. Guarded layouts return the full-column
case. `TornReads::prepare` calls the pure query once after the existing small-dense
admission and caches it with the matching layout and Jacobian read map. Layout
construction does not eagerly allocate an n-by-k support matrix; this avoids a
preparation regression for large candidates that will never use this path.

Supported destination columns execute every original dependency term in order,
including zero-valued operands, followed by the original pivot division. In an
unsupported column, the issued unguarded order proves every operand is zero.
Fresh checks establish finite coefficients and a nonzero pivot. Starting at
+0.0, subtracting their signed-zero products retains +0.0 under normal rounding;
one shared `0.0 / pivot` therefore reproduces each omitted cell, including -0.0
for a negative pivot. The reduced matrix still evaluates every original term;
nonfinite residual coefficients cannot disappear through this optimization.

Tests cover transitive support, signed-zero and supported arithmetic bits,
guarded full-column evaluation and refusal, nonfinite residual coefficients,
reduced operation count, and replacement of a same-sized layout with a different
tear order. Final worker checks: 344 Solve-IR library tests, 589 solver library
tests, strict all-target/all-feature clippy and workspace formatting pass.
Logs: `/tmp/rumoca-fourbar-recovery-support-logs/*postlint.log`.
Independent source review accepted `985c5f4b6`.

The retained source-bound `3b777` Fourbar layout has 616 coordinates and 16 tears.
Read-only reachability predicts 3,441 products and 2,511 divisions instead of
16,080 and 9,600 in its recovery recurrence. This is historical-layout static
work accounting, not a speedup measurement. The reproducible census and source
digest are in campaign `fourbar1-study/recovery-support-census.{py,json}`.
At combined source `98fce614f`, canonical focus recorded Fourbar 6.225987196 s
versus 6.188173533 s at `7143b62d`, with byte-identical traces (SHA-256
`7f0ebb08c690e4654500d632becfd2a34c53dde069ead992d3cfd3b15ee40315`).
This single observation establishes no material gain or statistically stable
regression. Focus retained 3 compared/high models, one exclusion and no missing
traces, but lost the excluded Thyristor model to its 12-second budget. The fixed
canary retained nine compared/high models. No full sweep or baseline promotion
was performed for this experiment. The next performance direction is reducing
repeated certified projections rather than accepting extra recovery metadata
without a demonstrated end-to-end benefit.
