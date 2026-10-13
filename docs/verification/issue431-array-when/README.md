# Checked array coordinates in when-equations

Issue [#431](https://github.com/CogniPilot/rumoca/issues/431) reports
`ScalableTestSuite.Elementary.WhenEvents.ScaledExperiments.ManyEvents_N_1000_M_10`
from casella/ScalableTestSuite revision
`c9a7574b31142fa976d20ab6970d78639c125ceb`.
The model retains one Boolean array `e[N]` and gives each selected element
its own event `when x[i] > 1 then e[i] = true`. MLS §8.3.5.3 requires
parameter target subscripts; §8.3.5.4 permits disjoint element definitions
and prohibits overlapping definitions.

The first divergent producer was Flatten: its assignment target extractor
converted the resolved parent and subscript into the rendered name `e[1]`.
ToDAE owns one aggregate coordinate `e`, so its exact role lookup correctly
refused the rendered element as ED019. Accepting that string in the validator
would leave whole-coordinate B.1c constructors and event ownership unable to
distinguish independent elements. Parser failure, zero event count, and a
missing declaration are rejected hypotheses: the bounded source closure
reaches ToDAE, contains the aggregate Boolean declaration, and contains the
source event chains before the failing role lookup.

The correction retains a Flat `AssignElement` with resolved parent identity
and structured parameter subscripts. ToDAE derives exact source keys from
`InstanceId`, checks complete rank and bounds, and constructs an inseparable
checked DAE selection. Whole declarations remain aggregate coordinates.
The selected B.1c constructor requires disjoint complete coverage and exact
current-value predecessors. Untouched elements retain explicit `pre`
definitions, using the source declaration's initialization. Solve keeps one
declaration history run with a checked partition into exact producer ranges;
independent elements retain their individual activations and holds.

The PR is stacked on PR #421 at
`bd6b6da89e0fcf63f6c739d9f4d6ff4f18823483`. The earlier `f7b18ac0`,
`eb515d9b`, `da102156` and
`6ce369dd` compiler snapshots are historical evidence only.
The clean historical `da102156` baseline independently reproduces ToDAE ED019
for `e[1]` on the exact pinned model.

The full upstream source-root reader fails earlier on an unrelated UTF-8 BOM
in `Mechanical/FlexibleBeam.mo`. Focused reproduction therefore uses a
bounded five-file closure containing the pinned package, Elementary package,
unmodified `WhenEvents.mo`, and MSL 4.1.0 `Modelica/package.mo` and `Icons.mo`.
The manifest records the exact file hashes. No equation or model body is
changed. All machine-local artifacts are under task-owned directories
derived from `$HOME`; raw paths in the receipt are relative to `$HOME`.

The [source manifest](source-closure-manifest.json) pins all five input files.
The historical da102 N1000/M10 run compiled in 84.91 ms on this host, retaining
one aggregate state declaration and one compact continuous equation family.
Its scalar balance has 1000 states, 1000 continuous equations, and 1000
discrete definitions; structural matching covers all 1000 state coordinates.
The bounded simulation completes to 0.95. At that terminal sample,
all 1000 Boolean coordinates equal OMC exactly; only `e[992]` through
`e[1000]` are true. Four rounded diagnostic Real samples differ from OMC by at most
`4.45e-13`. The [comparison receipt](omc-terminal-comparison.json) records
this terminal-sample scope; this is not a full trace-parity measurement.
The source experiment's requested horizon 10 also completes, in 5.15 s
on this host. All 1000 terminal Boolean coordinates equal OMC exactly,
including the 99 fired elements `e[902]` through `e[1000]`; the four rounded diagnostic Real
samples differ by at most `1.28e-13`. Its separate
[receipt](omc-terminal-comparison-10.json) retains the horizon and scope.

The compiler's focused fifteen-test suite proves aggregate storage, independent
triggers, partial held coverage, Integer/enum/discrete Real coordinates,
matrix mapping, parameter expressions, empty domains, dependency ordering,
and refusal of invalid coordinates, cycles, overlapping owners, and branch
mismatch. Constructor tests separately check forged wire selections and
partitions, incomplete coverage, shape, clocks, dependencies, and rollback.
Three simulation regressions pass for independent and simultaneous Boolean,
Integer, enum, and discrete Real updates; untouched elements; selected
current-value reads; elsewhen priority; and matrix row-major mapping.

This record is a focused verification ledger. Canary and cohort measurements
must retain their exact scope and reference context; partial snapshots must
not be promoted or reported as full-cohort parity.

The refreshed compiler on the historical eb515 stack repeats the requested
horizon 10 after the review corrections. It compiles in 179.58 ms and
simulates in 6.50 s on this host; these observations are not a performance
parity claim. All 1000 terminal Boolean values still equal the unchanged
OMC oracle, and the maximum rounded diagnostic sample difference remains 1.28e-13. The
[eb515 terminal receipt](omc-terminal-comparison-eb515-10.json) includes
both raw log and oracle hashes.

Source review added original scalar index-type and when-target variability
proofs before normalization, rejecting Real indices and dynamic Integer
indices. Selected current reads with parameter indices now freeze the same
coordinate used by dependency ordering and emitted values; their parameters
carry translation-selection metadata and the existing WD001 refusal.
`pre`, `previous`, and `sample` keep their temporal selection semantics.
The shared constant interpreter owns structural Integer evaluation, with
parameter/constant bindings and exact Integer results. Whole assignments,
function output tuples, and canonical expanded record receivers share source
coordinate overlap checks. Independent review confirmed the refusals and
positive parameter dependency, priority, and record tuple simulations.

Current compatibility probes pass for the immutable modelica_models revision
`a41f7c0c00b55c1bf54f03c9b66b901ba8e43c6f`: `Tests.All` compile and
`Tests.LieGroupTests.SO2` / `SE2` simulation at time zero. The CI-pinned
ModelicaTest overlay is revision `8ae3d35c24e519cb2996cab20f3b13daf2b0c50a`,
archive SHA256 `b402903aefd4f1397364e3ae1992bb99080ce935ab9a426d3bcdb47e4da8eeb6`.

The [canary ledger](canary-ledger.json) retains the failed first eb515
attempt (two ET009 enumeration-domain refusals) and its unmatched reference
context. The correction retrieves package-qualified table domains by their
resolved declaration identity. After that correction, the clean eb515 pair
has zero stage, completion, or accuracy-count deltas: 20 through Solve,
16 simulation completions, 15 compared/high, one typed skip, no missing,
near, or deviating model traces. Both reference keys are identical after
proving all 2669 `.mo` files byte-identical and preserving the baseline source
snapshot's timestamps in the owned cache. The key includes timestamps, so
this is an explicitly recorded context correction as well as a source fix.
The CI-selected `ModelicaTest.Blocks.MuxDemux` gate also passes: one selected
model compiles, simulates, and compares high, with required selected success.

The historical f7 source checkpoint `3c7023765517628782d22049620149c710afdf28`
repeats the exact N1000 horizon 10 model: 78.99 ms compile, 4.18 s simulation,
balance 0, 1000/1000 matching and zero terminal Boolean mismatches. The
[f7 terminal receipt](omc-terminal-comparison-f7-10.json) identifies the compiler
binary and raw log/oracle hashes. Real samples use the profile's 12 decimal
diagnostic formatting; their reported differences are not unrounded solver
error bounds. All three pinned modelica_models probes repeat successfully.
The f7 canary after and ModelicaTest receipts are now **unverified**: restoring
old timestamps on changed source after the baseline build did not establish
the intended compiler identity. Their reported counts are retained, but
excluded from PASS and parity claims. The first bd6 after attempt had the
same invalid cache reuse and is retained separately. All changed issue
source files were then invalidated, and the build logs confirm that the
affected IR, producer and consumer crates actually recompiled.

The current bd6 source checkpoint `67707e72538f582584c966fc45eb46dc8f945d1c`
passes all fifteen focused compiler tests, three event simulations and
workspace formatting. The exact N1000 horizon 10 model compiles in 82.32 ms
and simulates in 4.19 s: balance 0, 1000/1000 matching, all 1000 terminal
Boolean values equal OMC, with 99 true elements. The
[current terminal receipt](omc-terminal-comparison-bd6-10.json) records
the binary and raw log/oracle hashes and the rounded Real diagnostic scope.
All three pinned modelica_models probes repeat successfully.

The valid current bd6 fixed20 pair uses clean source and the same `ec011...`
reference context: 20 through Solve and balanced, 16 simulation completions,
15 compared/high, one typed skip, and no missing, near or deviating model
traces. Pipeline/completion/model-band deltas are zero, and no per-model
phase/error/simulation/IC status changes occur. The required-selected
ModelicaTest gate uses the same proven rebuilt compiler and passes one
compile/simulation/high comparison. These are partial cohorts; full
workspace, full release MSL and coverage/landing gates remain incomplete.
