# FMI tensor slice reads and updates

Governing contracts: MLS 3.6 sections 10.5, 10.6.1 and 11.2.1 (array indexing,
elementwise assignment and evaluating the right-hand side before storing it),
SPEC_0007's checked FMI/codegen boundary, SPEC_0032 section 4 (aggregate-native
updates), and SPEC_0033 sections 2, 3 and 6a (first divergence and verification).

The concrete failure is the connected `Bundle.Experiment` from bundle_web
commit `d2e3df65cd3f88812e280bc9dc5ab671d489539d`, assembled with the unchanged
shared Modelica library. Rumoca baseline is
`2aaed750242f96d77f39e319f16b24bee82794c1`.

Its DAE and checked FMI C profile construct successfully. Rendering
`rmc_functions.c.jinja` then fails in `typed_tensors.jinja` with
`unsupported-feature:fmi.c.pure-call: tensor update_slice`.
A function containing `y := x; y[2:3] := {20,30};` reproduces the same failure
without the application's physics, events, or estimator.

The first divergence is target rendering. Solve already owns the operation's
typed aggregate, replacement tensor and zero-based checked origin;
`SolveProgramBuilder::update_slice` proves matching element types, matching
rank and bounds. Native/WASM execution already implements that operation.
Changing array-shape inference, Modelica source, event admission or bounds
validation would target a different layer and is unnecessary.

Adding slice updates makes the reduced function package successfully; the
unchanged complete experiment then reaches the adjacent missing operation,
`tensor project_slice`. That operation likewise already carries checked origin
and output shape; the C renderer must read it rather than refusing it.

The C renderer maps each row-major slice element to its checked aggregate
origin using the retained shapes. An update first copies the original aggregate
and then stores the replacement; a projection reads the original aggregate.
Empty slices emit no indexing loop, avoiding division by a zero extent.
No earlier validation or unsupported-operation refusal is relaxed.

The fixed twenty-model canary comparison is recorded below.

Focused-case isolation found two earlier, independent construction boundaries.
A function that stores `zeros(0,2)` in `updated[2:1,2:3]` is refused before
rendering with `pure-call argument or slot interface is invalid`. Direct
Integer/Boolean array function outputs in an algebraic model are refused as
`structured B.1c body does not have one proven affine scalar program`.
Neither is changed here. Nonempty Integer/Boolean slice stores are exercised
inside functions with Real outputs, so their typed internal operations reach
the C target. Empty slice execution is not claimed by this regression.

After implementing both slice operations, the unchanged Bundle experiment's
first target failure becomes `integer binary integer_modulo`. A separate
function `input Integer x; output Real y; algorithm y := mod(x,6);` reproduces
that renderer refusal. Governing arithmetic semantics are MLS 3.6 section
3.7.2: `div` truncates toward zero, `rem` follows the dividend's sign, and
`mod` follows the divisor's sign. The Solve typed operations already retain
integer domains; `rumoca-eval-solve/src/typed_program/number.rs` implements
the reference checked integer semantics. The C renderer is again the first
missing owner, so division, remainder and modulo must use integer arithmetic
and preserve zero-divisor and overflow refusals.

The complete source FMU now packages. Independent FMI 3.0.2 XSD and FMPy
metadata validation pass, but strict C compilation exposes two additional
generator defects: Real `min`/`max` helpers are referenced by typed pure calls
but omitted from `model.h`, and `rmc_init_tangent` leaves its `seed` parameter
unused when neither tangent updates nor refresh blocks exist. SPEC_0040
SOLVE-C66 requires the canonical Real extremum helpers, including their NaN
and signed-zero semantics. The existing helper-use scan visits scalar rows
but never visits the retained typed pure-call bodies. The checked FMI renderer
must include those typed bodies (and their nested regions) in its use query;
changing the operation or substituting C library extrema would violate that
contract. The unused parameter is a template spelling issue and needs only
an explicit void use, with no warning suppression or numerical change.


## Focused validation

All runs use `CARGO_BUILD_JOBS=4 RUST_TEST_THREADS=4 RAYON_NUM_THREADS=4`.
The FMI suite uses its strict prerequisite marker, pinned FMPy 0.3.30, official
FMI 2.0.5/3.0.2 schemas and VDMCheck 1.1.3, compiling C with warnings as errors.
No prerequisite checks are skipped.

- `cargo test -p rumoca --no-default-features --features fmu-packaging,template-runtime-tests --test suite_template_runtime cli_target_fmi::slice_updates -- --nocapture`:
  passes; rectangular and rank-three projection/update, multiple function
  results, overlapping reads, source preservation, internal Integer/Boolean
  copies, both FMI versions, both ME and CS.
- The same command filtered to `cli_target_fmi::integer_quotients`: passes;
  all divisor/dividend signs, exact divisions, zero dividends, native 32-bit
  parameter endpoints, 64-bit function arithmetic endpoints, zero-divisor
  refusal, overflowing quotient refusal and representable remainder/modulo
  even when the quotient would overflow. Lazy inactive quotient is not run.
- The same command filtered to `cli_target_fmi::real_extrema`: passes;
  extrema in nested conditional/fold bodies, finite values and signed-zero
  ties, both FMI versions and ME/CS.
- `cargo test -p rumoca-phase-codegen real_extremum -- --nocapture`:
  four tests pass, including all signaling/quiet NaN, signed-zero, infinity
  and ordinary helper pairs, and the typed-owner use query's Real/Integer
  distinction.

The unchanged Bundle source (SHA-256
`e0f7b5b73e008824496b23cd90b4b7019fed27087a4c234637393949a9760c9c`)
now packages as FMI 3 and compiles through FMPy/GCC 15.2.0 with
`--all-warnings --warning-as-error`. Its original model/build descriptions
pass the official XSDs and FMPy. VDMCheck3 passes on a copy with only vendor
Annotations removed, matching the existing suite's documented VDM mixed-text
parser limitation; the original descriptions are never rewritten for schema
or importer checks.

Independent FMPy Co-Simulation execution matches the browser's pinned
Modelica/WASM pipeline across four scenarios: live zoom at 20 m, mixed
tripod/standing/walking cameras at 100 m with custom calibration and noise,
one-camera geometry across the GPS noise rollover, and loss/recovery of three
camera observations. All 166 sensor and 33 image counts are exact, held
estimates are unchanged between image ticks, and validity flags match.
Largest estimate component difference is 6.715e-6 in invalid geometry;
the two wholly valid scenarios differ by less than 2.215e-10.
These are focused application comparisons, not a full MSL parity claim.
Browser loading of a complete FMU and the FMI-LS-Wasm clock profile are not
changed or validated by this patch.


## Fixed twenty-model canary delta

Candidate `5e57cf4016f3019d49b96da795f46524b1dbbcce` was checked from a clean
checkout against the existing clean baseline
`2aaed750242f96d77f39e319f16b24bee82794c1`, using the identical fixed roster:

```sh
CARGO_BUILD_JOBS=4 RUST_TEST_THREADS=4 RAYON_NUM_THREADS=4 cargo xtask verify msl-parity \
  --sim-targets-file infra/verification/msl-canary-20.json \
  --results-dir "$HOME/scratch/bundle_web/build/fmi-validation/canary-5e57cf401" \
  --stage-parallelism 4 --sim-parallelism 4 --no-remote-quality-baseline
```

The command passes. Every non-simulation phase has one 10-second attempt;
simulation has the 12-second solver budget and 14-second parent watchdog,
with no retries. [Structured delta and evidence hashes](fmi-operations-canary.json)
retain the exact twenty model names. After excluding only timing/speedup fields,
all native model rows, execution counts, comparator model metrics and numerical
summaries are identical to baseline. Counts remain 17 compiled/balanced,
15 initialized, 15 completed simulations, two solver failures, no non-finite
results and no timeouts. Three compile-stage failures are unchanged.

The comparator checks 14 models / 445 channels, with no bad channels,
zero missing traces, and one unchanged policy exclusion (the Digital Counter
OMC reference failure). All 14 state selections match exactly. These are
canary observations and a zero delta, not a 566-model cohort parity number.

The complete `cli_target_fmi::` suite additionally passes: 42 tests,
zero failures or skips (198.86 s). The four codegen extremum tests pass.
Full workspace checks and the coverage pre-landing gate have not yet been run;
this evidence does not authorize landing without SPEC_0025's required gate.
