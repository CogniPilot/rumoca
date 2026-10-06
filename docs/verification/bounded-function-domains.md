# Bounded function-domain preparation

Base revision: `37a875980a8e7478129b93a6dec960cca193eb68`.
Branch: `fix/slam-dynamic-function-loops`.

The unchanged full-raster reproducer is
`crates/rumoca-compile/src/session/tests/fixtures/feature_selection_full.mo`, copied from the editable
application source `models/FeatureSelection.mo`. Its SHA-256 is
`be0d24d9b48176b6dad8dc44ce19f1355268a62e5f6ea2fb6c607efd298ecc23`.
It retains 160 by 90 scores and capacity 14400, stable raster tie ordering,
ties-to-even ranks, in-place heap sorting and occupancy suppression. No host
algorithm, statement expansion or smaller replacement workload is introduced.

## First refusal and preparation ownership

The reviewed browser package at revision
`94cff417cec61c256b5ea307d5d2a7f8213437ac` rejects the full source in ToDae,
before solver preparation or native execution. The reported owner is
`function loop domain`: `SelectRasterFeatures` requires a compact
dependent-domain transition and scalar statement expansion is prohibited.
The complete source compile took 322.7 ms with 182036 KiB maximum RSS. Its WASM
SHA-256 was `9e3e0c1070b4d4fecb155ffe0ca4315be8f0de655534535ea85ea99eddbd20db`.
These are refusal measurements, not full-image numerical or throughput passes.

`analysis/loop_compaction/mod.rs::compact_function_loops` normalizes the body,
then refuses a surviving dependent range. `analysis/function_loops.rs` issues
only finite statically proven `StructuredIndexDomain` owners.
`analysis/function_bodies.rs` has no general While transition planner; the
special integer-reduction planner is not a general image algorithm owner.

## Supported compact profile and safety repairs

This change stages finite Integer ranges whose start and end have checked
conservative intervals, whose signed stride is constant and nonzero, and whose
operands remain invariant during the loop. A canonical ascending or descending
envelope retains source point order. An exact direction and congruence mask
executes only source points. Nonlinear bounds use checked interval arithmetic;
sampling syntax at endpoint values cannot prove interior extrema. Empty domains
remain empty, including descending domains. Compact storage is independent of
the 14400-point parent extent.

The existing interval propagation could label a mutable counter with the value
seen after visiting its loop body once. It also retained a previous finite fact
after an unknown write. Those observations do not prove bounds for all reaching
values. Recurrences and unknown multi-output writes now invalidate the fact;
loop binders are scoped and cannot replace an enclosing input's value. Exact
translation-time values remain separate from conservative intervals.

A source For range is evaluated on entry. If its body writes any range operand,
the current profile refuses with an entry-snapshot diagnostic. It does not read
that operand repeatedly as a substitute for the source range. General While,
dynamic strides, unproved mutable counter bounds, arbitrary break and dynamic
tensor addressing require their checked owners and remain unsupported here.

## Verification status

Staged differential tests compare the source and compact statements through an
independent total interpreter, including ordered recurrence, array aliasing,
positive and negative stride, empty domains and nonlinear interior points.
Separate actual compiler/evaluator fixtures check ordered scalar results past
ToDae. Interval regressions cover 14400-iteration counter unsoundness, indirect
recurrences, unknown overwrites, multi-output writes, binder shadowing and
checked arithmetic overflow.

The new interval regressions against the unchanged base produced 0 passes and
9 failures. The initial compact-domain differential regressions produced
1 pass and 4 failures. These baseline runs use the same new test bodies with
the original production implementation; initial harness setup failures are
not counted as behavioral evidence.

The final `rumoca-phase-dae` library suite passes all 312 tests. Its existing
independent generated corpus covers 40010 programs and compares 159741
source/rewritten pairs, with no output divergence or asymmetric refusal.
The 299 unexecuted pairs are explicit reference-interpreter refusals for
inexact Real arithmetic or checked Integer overflow.

Six actual Session compiler fixtures pass: ordered positive and negative
strides, nonlinear interior bounds, enclosing-binder shadowing, an explicit
entry-snapshot refusal and the unchanged full-raster refusal. Accepted fixtures
retain a compiler-issued compact Fold and check independent numerical results
at two function-input values. Their calling model uses a parameter overridden
by the static NumericEvaluator, whose coordinates permit Parameter/Constant
values. These are source-to-DAE/evaluator proofs, not browser Session runtime
or WASM throughput measurements.

Strict scoped Clippy passes for `rumoca-phase-dae` and `rumoca-compile`, including
all targets and features. The final library gate took 49.881 seconds including
compilation (11.27 seconds of tests); the compiler fixture gate took 115.474
seconds including compilation (0.05 seconds of tests); strict Clippy took
16.665 seconds. Peak observed aggregate process RSS was 1401400 KiB, available
RAM remained above 44 GiB and the private target cache remained below 4.3 GiB.
Each command used two jobs on two CPUs, a hard 8 GiB memory cgroup with swap
disabled, an 8 GiB aggregate-RSS watchdog, a 7.5 GiB cache watchdog and a
600-second deadline. Rustfmt and `git diff --check` pass.

The workspace architecture gate remains pending. A fresh standalone checker
must embed this worktree's `CARGO_MANIFEST_DIR`; a cached checker compiled for
another tree cannot establish these source checks. The initial standalone
attempt lacked matching dependency variants and Cargo compile-time environment.
A bounded plain `rumoca-compile` library build to supply compatible dependencies
reached the facade but hit its hard 180-second scope deadline. It observed
1386044 KiB peak RSS and a 4945772544-byte private cache. No architecture pass
is claimed. Broad workspace, documentation, MSL and ModelicaTest gates also
remain integration gates; the focused tests are not complete cohort evidence.

Full FeatureSelection remains unadmitted. Its 14400-pixel fixture is retained
unchanged and the application full-size actual-WASM gates are unchanged.
General While transitions, lifetime-safe dynamic array storage and native
FunctionFold execution still require their owning compiler/runtime work.

Applicable contracts: SPEC_0007/0040 compact function IR, SPEC_0032 range
ownership, SPEC_0036/0043 typed carried tuples, SPEC_0022 ALG-016 entry-evaluated
ranges and scoped break, SPEC_0029 producer boundaries, SPEC_0021 complexity,
SPEC_0033 earliest-owner proof and independent numerical validation.
