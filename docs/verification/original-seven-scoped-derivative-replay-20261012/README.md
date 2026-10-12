# Original seven-model scoped derivative replay

The historical 967e public formal constructor refuses all seven immutable DAE inputs with an escaped construction-issued conditional arm. Its actual error spans identify the nested Ramp value in `Modelica.Blocks.Sources` line 252 (Toroidal/SMPM) and the KinematicPTP condition at line 1225 (Cascade). The separate historical 88ae authored-source run failed all seven structural matchings. Those receipts are retained.

Combined source `be20244f409d7c8e984328dc9cb2953db8ffdae9`, which already includes the checked scoped derivative repair, admits all seven formal constructions on the identical inputs and completes all seven unchanged authored simulations. No additional production repair, model rewrite, matching relaxation or baseline exclusion was required. All 79 Toroidal source continuous rows have identical recursive operations, literals and exact declaration-ordinal reads across the old c851/current input bracket; source roles and empty initialization rows also match. New arm ownership is the relevant representation change.

| Original model | Full authored horizon | Trace samples | Channels | Integrated trace states | `sim_seconds` |
|---|---:|---:|---:|---:|---:|
| `Modelica.Clocked.Examples.CascadeControlledDrive.AbsoluteClocks` | 0–4 | 4007 | 89 | 2 | 3.169461 |
| `Modelica.Clocked.Examples.CascadeControlledDrive.Continuous` | 0–4 | 507 | 80 | 3 | 2.444473 |
| `Modelica.Clocked.Examples.CascadeControlledDrive.SubClocked` | 0–4 | 4007 | 89 | 2 | 3.304977 |
| `Modelica.Clocked.Examples.CascadeControlledDrive.SuperSampled` | 0–4 | 4007 | 89 | 2 | 3.133569 |
| `Modelica.Electrical.Machines.Examples.SynchronousMachines.SMPM_NoLoad` | 0–0.32 | 3218 | 459 | 1 | 5.768054 |
| `Modelica.Magnetic.FluxTubes.Examples.BasicExamples.ToroidalCoreAirgap` | 0–0.05 | 503 | 90 | 0 | 2.360778 |
| `Modelica.Magnetic.FluxTubes.Examples.BasicExamples.ToroidalCoreQuadraticCrossSection` | 0–0.05 | 503 | 90 | 0 | 1.889087 |

`sim_seconds` is the worker-reported field; its backend-build/run/wall components are separately retained in the receipt. No cold-build speed or speed superiority claim follows. Every retained trace is nonempty, rectangular and finite. Integrated trace state counts differ from the preselection source-state metadata.

The unchanged model phase 10 s, solver 12 s, simulation watchdog 14 s and source startup 60 s budgets apply. Guardian exit0 after 341.420 s including Rust builds; peak aggregate resident plus swap 2,796,552 KiB; no resource stop. Guards retain 600 s/4 GiB memory/16 GiB host reserve/8 GiB owned target/8 GiB scratch reserve. All tracked recipe input hashes were rechecked after execution.

The independent canonical OMC comparison is complete: all seven models have high agreement, all 656 shared channels are compared, no shared channels are omitted, and no bad or severe channels occur. All 656 initial values have high agreement. There are no excluded, skipped or missing models. Exact state sets match three models; state counts match all seven, with four alternate coordinate choices retained in the report. Native-only channels absent from OMC CSV are explicitly inventoried and are unmeasured by this comparison. The unchanged authored horizons apply; fresh OMC references use one worker, one thread and the existing 12 s per-model budget. OMC reference generation and comparison took 38.528 s; guardian exit 0 after 45.947 s, peak resident plus swap 674,276 KiB, no resource stop.

Worker source is be202; the sealed comparator is from source 117b2490b, with its exact artifact hash bound separately. The report’s runtime Git metadata identifies its execution directory revision, not the worker or comparator compiler source. No combined-source or performance claim is inferred from that field. This focused completion receipt is not a full 566-model cohort or a final-head/workspace qualification. `receipts.json` uses paths relative to `$HOME`, binds the raw artifacts and preserves the historical failed constructor separately.
