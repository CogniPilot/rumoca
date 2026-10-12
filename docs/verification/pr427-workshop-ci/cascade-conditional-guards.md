# Cascade drive structural guards

The four unchanged MSL 4.1.0 `CascadeControlledDrive` examples reproduce CI
job 114777990784 at `85c37de07f08a9c5fed578f70d9e60886559bda3`:
`AbsoluteClocks`, `SubClocked` and `SuperSampled` refuse with structural rank
44/46; `Continuous` refuses with 51/53. The sealed pre-normalization `8a0853`
CLI simulates all four successfully using identical model bytes and arguments.
This is a semantic regression, including a continuous controller, rather than
an MSL revision, missing tool or synchronous-clock-only failure.

Lazy scalar Boolean normalization correctly represents And/Or with checked
Conditional nodes. The structural `parameter_conditionals` consumer recognized
the former Boolean operations but omitted Conditional composition, withholding
the invariant/piecewise-constant guard proof used during differentiation of the
shared `KinematicPTP2` trajectory. The focused constructor BEFORE fails the
parameter-guard proof, while its unowned relation, noEvent and smooth refusal
controls pass. The repair composes the existing facts over every checked
Conditional operand. It retains the lazy producer, relation event ownership,
call-context restrictions, downstream checks and all model/oracle limits.

The owner remains phase-Structural under SPEC0007 and SPEC0029. MLS 3.6.5 and
8.5 govern the existing guard-preserving differentiation rule; SPEC0022
EXPR-042 requires preserving lazy Boolean semantics. SPEC0033's first-owner and
unchanged-model proof is provided by the constructor discriminator followed by
the actual four-model replay. No model-name branch, new evaluator or fabricated
event owner is introduced.

At repaired source `e6b21c70357b6b997ceea004104e8cfc60af6748`, the focused two
tests, full structural library (290 tests), scoped all-target/all-feature
Clippy and whole-workspace formatter all pass. All four original simulations
reach authored time 4 with finite CSV values, and each CSV is byte-identical to
its actual same-input `8a0853` simulation. The CLI reports baked identity
`e6b21c70357b`; its SHA256 is
`acf6d9b87c805b968db6342273f1884433c9484907773ef2b067e8b328749cc8`.
All 4,217 tracked inputs and 2,946 pinned MSL inputs were checked after execution.

| Original example | Simulate wall seconds | Common OMC channels | Median bounded L1 | Mean bounded L1 |
|---|---:|---:|---:|---:|
| AbsoluteClocks | 13.542 | 42 | 4.674e-6 | 2.094e-4 |
| Continuous | 6.578 | 32 | 3.019e-7 | 8.903e-6 |
| SubClocked | 9.684 | 42 | 6.296e-6 | 2.306e-4 |
| SuperSampled | 7.334 | 42 | 6.296e-6 | 2.306e-4 |

The unchanged `rumoca-msl-tools plot-compare --reuse-traces` compares the full
0..4 horizon against sealed old-cohort OMC references (DASSL, tolerance 1e-6,
500 intervals, OMC `a96aa1a-cmake`). An external adapter only parses CSV into the
existing trace schema; it implements no numerical comparison. It carries no
variable metadata/certification profile. Three OMC-only channels per model
remain unmatched; exact common and missing names are retained in the hashed
input report. The comparator ELF was copied read-only from the existing Root
MSL tool cache and is hash-bound, not claimed as a newly built repaired-tip
artifact. These are measured focused comparisons, not strict-high, full-cohort,
canonical quick/full or CI qualification.

The cold BEFORE build took 554.668 seconds; the AFTER tests/lint/build/four
simulations took 287.398 seconds. Both retained the same 600-second outer limit,
4 GiB aggregate RSS+swap cap, 8 GiB target/scratch bounds, 16 GiB host reserve and
4 workers. No model settings, tolerances or watchdogs changed. The early
`8a --inspect structure` route also refuses these models, but differs from
actual simulation preparation; it is retained as auxiliary evidence and does
not replace the actual old-CLI simulation verdict. One missing-copy attempt
executed no old model and remains a separate infrastructure receipt.

[The portable receipt](cascade-conditional-guards.json) binds source, binaries,
models, guards, traces, reference copies and raw logs. Raw evidence stays at
`$HOME/scratch/rumoca/pr427-cascade-clocked-structural-20261012/evidence`;
the original receipt SHA256 is
`97a877240f13775e3b1ee11c12f6b7a5016689cd8b4d151a52fc6dbb85f0056c`.
