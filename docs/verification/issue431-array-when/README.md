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
`da102156cf4496e8a2255e905ebe66b859e339db`. An earlier diagnostic baseline at
`6ce369dd2f4c961704a05c9f3163cda90480fb79` is historical evidence only.
The current clean `da102156` baseline independently reproduces ToDAE ED019
for `e[1]` on the exact pinned model.

The full upstream source-root reader fails earlier on an unrelated UTF-8 BOM
in `Mechanical/FlexibleBeam.mo`. Focused reproduction therefore uses a
bounded five-file closure containing the pinned package, Elementary package,
unmodified `WhenEvents.mo`, and MSL 4.1.0 `Modelica/package.mo` and `Icons.mo`.
The manifest records the exact file hashes. No equation or model body is
changed. All machine-local artifacts are under task-owned directories
derived from `$HOME`; raw paths in the receipt are relative to `$HOME`.

The [source manifest](source-closure-manifest.json) pins all five input files.
The exact N1000/M10 model now compiles in 84.91 ms on this host, retaining
one aggregate state declaration and one compact continuous equation family.
Its scalar balance has 1000 states, 1000 continuous equations, and 1000
discrete definitions; structural matching covers all 1000 state coordinates.
The bounded simulation completes to 0.95. At that terminal sample,
all 1000 Boolean coordinates equal OMC exactly; only `e[992]` through
`e[1000]` are true. Four sampled Real coordinates differ from OMC by at most
`4.45e-13`. The [comparison receipt](omc-terminal-comparison.json) records
this terminal-sample scope; this is not a full trace-parity measurement.
The source experiment's requested horizon 10 also completes, in 5.15 s
on this host. All 1000 terminal Boolean coordinates equal OMC exactly,
including the 99 fired elements `e[902]` through `e[1000]`; the four Real
samples differ by at most `1.28e-13`. Its separate
[receipt](omc-terminal-comparison-10.json) retains the horizon and scope.

The compiler's focused nine-test suite proves aggregate storage, independent
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
