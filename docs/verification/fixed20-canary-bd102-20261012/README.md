# Fixed 20-model Tier 1 canary at bd1027a42

The exact committed canary roster has **15 simulation completions, 14 high-agreement comparisons across 445 shared channels, and five failures**. There are zero new or recovered native failures compared with the retained canary at `13429f49687711a39a8fb63befc9c487544ac5a6`. Every native status and error string matches. The high-model set, each compared model’s shared-channel count, and the existing Counter reference exclusion also match. This is a focused tripwire, not a 566-model cohort result or an all-green canary.

The current native source is `bd1027a423d536dfff76c56f161ff606e557aaba`. The sealed OMC/comparison executable is from the distinct source `117b2490b`; its full SHA256 is retained in the receipt. Both use the pinned MSL 4.1.0; OMC reports `a96aa1a-cmake`. The canonical roster and committed corpus-pin bytes are identical to the historical run. The historical library directory has since been retired, so its bytes cannot be freshly rehashed. The historical harness used msl-fast/four workers; this run used an optimized dev worker/one worker. No performance delta is claimed.

## Actual execution

All 20 models received one native attempt with 10-second phase, 12-second solver, 14-second parent simulation, and 60-second startup budgets. No model was retried or removed. Requests match the historical canonical fields except owned input/output paths. The native guardian exited 1 because five models failed, in 18.922 seconds, with a 1,359,712 KiB peak aggregate RSS plus swap and no resource stop. All 15 completed traces are independently finite and rectangular. Tracked source, sealed binary, and full pinned-library manifests passed before/post checks.

Fresh OMC attempted all 20 models once: 19 completed, and OpAmpCircuits.Add failed Pantelides index reduction. A separate comparison carrier contains the exact 20 issued worker records. The existing canonical tool’s `--rumoca-sim-ok-only` route reused all 15 successful fresh OMC artifacts, with no OMC process or model rerun. It compared 14 models with zero bad/severe channels and zero missing or nonidentifiable traces. Counter remains the same checked reference-generation policy exclusion and receives no high credit. Its exclusion concerns the OMC default-grid delayed flip-flop event result; no horizon/grid adjustment was used here.

## Preserved harness failures

The first diagnostic invocation mistakenly enabled full IR JSON emission. Its 20 outcomes remain: 13 simulation completions, the five historical failures, and Counter/FullAdder timeouts in ArtifactWrite before simulation. This run is **not** the canonical canary. The corrected run restored the actual canonical `emit_json=false` setting; both models then completed.

Two OMC preflights lacked staged MSL/ModelicaServices paths and attempted zero models. Their receipts are retained. The first full OMC invocation completed its 20-model roster but compared zero models because the direct-worker carrier lacked `msl_results.json`; it is explicitly parity unmeasured. Supplying the exact worker records enabled a separate cached comparison without reexecuting any model. These setup failures are not silently replaced by pass claims.

## Model outcomes

| Model | Native result | Comparison |
|---|---|---|
| `Blocks.Examples.BooleanNetwork1` | ToDae | native_failure |
| `Blocks.Examples.Noise.AutomaticSeed` | Flatten | native_failure |
| `Clocked.Examples.Elementary.BooleanSignals.BackSample` | sim_ok | high (10 channels) |
| `Clocked.Examples.Elementary.BooleanSignals.TimeBasedPulse` | sim_ok | high (3 channels) |
| `Clocked.Examples.Elementary.IntegerSignals.BackSample` | sim_ok | high (10 channels) |
| `Clocked.Examples.Elementary.IntegerSignals.TimeBasedStep` | sim_ok | high (3 channels) |
| `Clocked.Examples.Elementary.RealSignals.Sample1` | sim_ok | high (5 channels) |
| `Clocked.Examples.Elementary.RealSignals.TickBasedRamp` | sim_ok | high (3 channels) |
| `Clocked.Examples.SimpleControlledDrive.ClockedWithDiscreteController` | sim_ok | high (26 channels) |
| `Electrical.Analog.Examples.DemoPowerSupply` | sim_ok | high (21 channels) |
| `Electrical.Analog.Examples.OpAmps.Comparator` | sim_ok | high (45 channels) |
| `Electrical.Analog.Examples.OpAmps.OpAmpCircuits.Add` | sim_solver_fail | native_failure |
| `Electrical.Analog.Examples.ShowSaturatingInductor` | sim_ok | high (23 channels) |
| `Electrical.Digital.Examples.Counter` | sim_ok | policy_excluded |
| `Electrical.Digital.Examples.FullAdder` | sim_ok | high (194 channels) |
| `Electrical.Polyphase.Examples.TransformerYD` | sim_solver_fail | native_failure |
| `Math.Nonlinear.Examples.QuadratureLobatto3` | sim_ok | high (3 channels) |
| `Mechanics.Rotational.Examples.FirstGrounded` | sim_ok | high (46 channels) |
| `Mechanics.Rotational.Examples.Friction` | ToDae | native_failure |
| `StateGraph.Examples.ShowCompositeStep` | sim_ok | high (53 channels) |

The three compiler refusals are BooleanNetwork1 (nested-return control-flow owner), AutomaticSeed (unordered impure library state), and Friction (104 equations/102 unknowns). The two Solve failures are OpAmpCircuits.Add (57/58 structural match) and TransformerYD (non-numeric affine derivative coefficient). They remain visible and do not receive simulation or comparison credit.

`receipts.json` binds exact recipes, commands, raw logs, guards, requests, source/library manifests, actual outcomes and comparison reports with SHA256 digests. Machine-local raw directories are recorded using `$HOME`. No compiler/runtime source change, baseline reset, tolerance change, or issue closure accompanies this evidence.
