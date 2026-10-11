# Checked conditional differentiation qualification

The unchanged MSL 4.1.0 `DemonstrateLightning` and `ResonanceCircuits`
models failed structural reconstruction on source
`4aaaf7872ece5d60c13217565ae4bd5a4aa450c6`. A temporary diagnostic identified
quotient differentiation creating arithmetic outside the source value's issued
conditional arm. The temporary changes are absent from the repair.

The repair uses the same checked begin/arm/finish implementation from both
construction facades. Transformed values receive fresh arms; closed source arms
remain closed. Reconstruction caches include the exact target arm. Reuse of a
global mapping additionally checks that the actual target value is global:
the first repair exposed a sibling-arm coordinate leak, which the follow-up
corrected without relaxing visibility checks.

This implements the ownership contracts in SPEC_0007/SPEC_0040 and the shared
owner requirement in SPEC_0029. Selected-value differentiation retains MLS
§3.6.5 and §8.5 semantics. Tensor values remain aggregate values; the repair
adds no per-point expansion, alternate evaluator, wire change or schema adapter.

| Actual qualification | Result |
| --- | --- |
| Nested conditional quotient constructor on test-only `ed87b31d1` | 0/1, escaped arm |
| First repair `c9817f9c1` | 0/1, sibling-arm cache leak |
| Corrected repair `c4ac96425` | 1/1 |
| Full IR-DAE and structural libraries at `63061eb28` | 224/224 and 270/270 |
| Scoped all-target/all-feature Clippy at `487b46439` | Passed, `-D warnings` |
| Unchanged Lightning and Resonance at `487b46439` | Both compiled through structural reconstruction to Solve JSON |
| Existing event-guard, tensor-parameter-branch and piecewise-state regressions at `487b46439` | Three exact selectors, each 1/1 |

The three existing public tests retain their numerical/shape assertions and
tolerances. Each selected one test, with 1,450 filtered out. The original MSL
commands establish compilation to Solve IR; they do not establish simulation or
OMC parity. These results do not claim the full workspace or remote CI is green.

The adjacent JSON binds commands, raw logs, guardian reports, the sealed CLI and
Solve outputs by SHA256. Raw evidence is retained under
`$HOME/scratch/rumoca/pr427-owned-conditional-derivatives-20261012/evidence`.
All tracked source bytes match the frozen-build manifest after qualification.
The manifest was observed during the CLI build, not before its launch.
The 600-second, 4-GiB resident-plus-swap, 8-GiB cache and scratch-floor guards
were unchanged; successful runs reported no resource stop.
