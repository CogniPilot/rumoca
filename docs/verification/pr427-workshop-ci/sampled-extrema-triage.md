# Sampled extrema: incorrect timestamps

The complete 566-model comparison at
`86c3bc1a6128e3d3174fefa169ae7bd5e03243e9` found a trajectory deviation in
`Modelica.Blocks.Examples.DemonstrateSignalExtrema`: its four `t_min`/`t_max`
channels do not update correctly. Of 319 simulation completions, OMC compared
270 models (269 strict-high, one deviation); 48 were skipped and one lacked a
reference trace. This run failed its quality gate.

## Concrete reproduction and controls

[The small original reproduction](sampled-extrema-reproduction.mo) samples
`u=time` every 0.1 seconds, updates `y_max`, and records the time in an
if-equation when that maximum changes. The sealed CLI built at `1dfdf57`
(SHA-256 `98cead48e63201f29a513bf849212a6e174c4909afa6b3d856d1992644f05b52`)
reproduces the failure with BDF. Its production sources match the later
`86c3bc1a` checkpoint; the intervening commits only refactor test helpers.

| Right-limit time | Rumoca maximum | Rumoca timestamp | OMC timestamp |
|---|---:|---:|---:|
| 0.1 | 0.1 | 0 | 0.1 |
| 0.2 | 0.2 | 0 | 0.2 |
| 0.3 | 0.3 | 0 | 0.3 |

OpenModelica `a96aa1a-cmake` ran the same source with DASSL and completed
successfully. Explicit `noEvent` around the body comparison still produces the
wrong Rumoca timestamps, while expressing that same conditional as
`t_max = if y_max > pre(y_max) then time else pre(t_max)` produces the correct
timestamps. The first if-expression control attempts refused duplicate sibling
class definitions; the successful control uses an isolated directory and a
distinct model name. Those refusals are not numerical test outcomes.

## Producer ownership

MLS §8.5 treats when bodies with implicit `noEvent`, including body
if-equations ([language specification](https://specification.modelica.org/maint/3.6/equations.html#events-and-synchronization)).
Two compiler differences are visible before execution:

- The ordinary body condition calls activation lowering and issues an
  unnecessary relation/root. The explicit-noEvent control has no root and
  still fails, so removing that root alone cannot close this counterexample.
- `SameTickExchange::derive` classifies both source triggers and body guards
  as entry reads, issuing the timestamp producer before the maximum producer.
  The equivalent if-expression is a value read and receives the opposite,
  correct producer order.

The proposed SOLVE-C58 amendment distinguishes source activation triggers from
body guards. The candidate reuses `lower_body_value` for literal body conditions
and the existing structural dependency collector, pruning each branch's own
trigger subtree by checked condition identity. It preserves coordinates that
also occur in body guards, alias closure, entry-trigger ordering, and the
existing policy for observation cycles. It adds no runtime reconstruction or
model-specific branch.

## Qualification status

At frozen before source `b6dd43d0c64d54844627ee7208aff8ed3de86823`, all three
public extrema tests fail: the body comparison issues a crossing root and the
timestamps stay at zero. At frozen after source
`117b2490b79a081fd21e87e028f19a29e5636296`, all five public tests pass with
Interpreter and Auto execution. They cover the original and explicit-noEvent
extrema values, absence of crossing roots, a body assertion that holds, and
an assertion that fails at exactly 0.1 with its original message through the
common typed Model Exchange boundary. Three structural identity tests and all
60 selected existing activation/history regressions also pass (68 total).
Tracked-source manifests before and after these commands are identical.

The after CLI, SHA-256
`4035d88fc8faf1875ef3486cab09e8b5ed8b38281dcb0de195f4ab70452b2358`, also
completes the unchanged original MSL example through 1.5 seconds at output
interval 0.00001 and absolute/relative tolerance 0.000001. Its finite,
rectangular trace has 165001 rows, all 150001 regular output coordinates,
and 18 channels. A separate sealed native worker from the same source also
completes under the canonical 10-second Solve and 12-second solver budgets:
Solve takes 0.003119188 seconds, backend preparation 0.019161915 seconds,
and the solver 1.864257443 seconds. Its metadata-bearing trace contains
165001 rows and 18 variable descriptions, with finite samples throughout.

The unchanged canonical OMC comparator then compares the original cached
reference against that worker trace: one model compared, zero skipped and
zero missing, all 16 shared channels strict-high, zero deviation channels,
and all 16 initial channels high. The comparison has 6408000 paired samples.
Two internal amplitude/frequency channels occur only in Rumoca;
`der(sine.phi)` occurs only in OMC and is not compared. State-selection
agreement was not measured. Source manifests match before/after and the OMC
reference bytes are unchanged. The [portable qualification receipt](sampled-extrema-original-qualification.json)
records the exact source, sealed ELF/artifact hashes, worker result and
comparator counts. This is a focused full-original-model comparison,
not cohort parity. The fixed canary delta and fresh complete 566-model
comparison remain pending; this repaired model must remain in that sweep.

The original-model guardian receipt is retained: elapsed 15.809 seconds
including CLI startup/compilation, peak aggregate resident-plus-swap 1809124
KiB, no resource stop, process exit zero. This diagnostic used a 30-second
outer command bound; it does not establish the canonical 12-second solver
budget. The focused 68-test run printed guardian receipts to the orchestration
output without saving separate JSON files. Its raw Cargo logs and source
manifests are retained; no complete per-command resource receipt is claimed.
The proposed SOLVE-C58 clarification remains subject to the required spec/PR
review; passing tests do not constitute approval.

Raw before traces, DAE/Solve wires, OMC script/results, source manifest and
bounded test receipt are retained under
`$HOME/scratch/rumoca/workshop427-extrema-event-20261012/evidence`.

Original-model source/ELF seals, CSV audit and guardian are retained under
`$HOME/scratch/rumoca/workshop427-signal-extrema-after-plan-20261012/evidence`.
