# Conditional SSA stack and shared-DAG repair

Production checkpoint: `cd1fca7689e80a7080aa1e5c3a416a91f9157079`.
The owning phase is `rumoca-phase-solve`; SPEC_0007, SPEC_0029 and SPEC_0033
apply. MLS §3.6.5 and §12.4.1 govern conditional evaluation and sequential
function assignments.

## First divergent owner

The Windows 3,000-assignment control exposed scalar reads that bypassed the
existing iterative conditional-definition emitter. Routing every scalar
function conditional through a separate checked program fixed that stack
failure at `8b95e49853ec`, but duplicated shared expression graphs between
independent regions. CI then timed out lowering
`Modelica.Media.Incompressible.Examples.TestGlycol`: about 0.084 seconds before
the change, versus the unchanged 20-second Solve limit afterward.

The repair routes conditional RHS reads through `pack_function_definition` at
their actual SSA definition owner, after existing capture/group handling.
It reuses checked scalar offsets and the existing context restoration/cache
path. Direct expression diamonds retain the flat shared expression cache.
No new IR constructor, cache, guard hoist, validator exception or budget is
introduced. Observational controls found all 1,750 old-path guards unknown;
guard pruning could not explain its speed. Capture-only sharing and an existing
totalness dispatch split still timed out and were discarded.

## Matched controls

All local Cargo commands used four build jobs, four test threads and four Rayon
threads. Evidence is in `$HOME/scratch/rumoca/ci-repair/`.

| Control | Old or broken path | Repaired path |
| --- | --- | --- |
| Identical saved Glycol DAE | Old-dispatch Solve 0.071963 s | Solve 0.060125 s; byte-identical 2,157,607-byte Solve JSON |
| Identical saved rectifier DAE | Old-dispatch Solve 0.061525 s | Solve 0.062753 s; byte-identical 1,015,144-byte Solve JSON |
| Shared diamonds, depth 4 → 8 | Exact `8b95` compiler: 1,238 → 100,598 operations; growth assertion fails | 58 → 106 operations; numerical residual and scalar-path census pass |
| Unchanged 3,000-link source | Old scalar dispatch overflows its 256 KiB lowering stack | Passes at the same bound, at least 3,000 checked conditional owners, result 3,000.5 |

The diamond regression constructs checked DAE and counts both scalar conditional
forms. The long chain separately requires checked conditional owners. Neither
test can pass by bypassing the scalar path. The final integration conditional
suite passed 50 tests, including the chain in 24.57 seconds. Other passing
filters cover definedness (33), function assertions (13), branch assertions (5),
read-before-write (5), Integer (19), Boolean (13), additional source order (3),
phase conditional/AD (12) and phase Integer owners (8). Commands use
`cargo test -p rumoca --test suite_core <filter>` and
`cargo test -p rumoca-phase-solve <filter>`; strict phase-solve all-target Clippy,
formatting and whitespace checks pass. Logs use the `definition-owner-` prefix;
the negative growth receipt is `definition-owner-dag-8b-control.log`.

EngineV6's matched old/repaired Solve phases completed in 17.8227/17.2106 seconds,
then both diagnostic exports hit the existing 32-MB output ceiling. Its IR
equality and full simulation remain unmeasured by that diagnostic. The remote
EngineV6 and rectifier timeouts remain open until final CI clears the unchanged
full-cohort gate; a short diagnostic run is not cohort evidence.

## Other CI repairs and remaining gates

The sampled-event coverage fixture now checks a refreshed algebraic assignment
and its `assignment_schedule` decline receipt. Strict rustdoc uses code text for
the private, cfg-gated provenance helper. The landing gate inherits the caller's
build budget instead of overriding it with four duplicated `-j 8` arguments;
its 11 existing tests pass.

Final-head fixed-20, committed-snapshot coverage gate and remote CI are required
before landing. Prior-head canaries or passing individual jobs do not qualify
this checkpoint; no successful full-cohort or final landing-gate claim is made
here. No quality baseline, exclusion, timeout, stack or memory limit changed.
