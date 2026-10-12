The new ordinary `suite_core` test `msl_glycol_viscosity_uses_its_checked_coefficient_interface` is unrun. Its source base is `c85128c50cd6e65198a712578c80679afc01aeaf`. It compiles the unchanged MSL `Modelica.Media.Incompressible.Examples.TestGlycol` and an independent public `Medium.dynamicViscosity(state)` call using the existing mandatory MSL fixture helper. Both must produce exactly three samples at 0, 0.005 and 0.01; every recorded value must be finite. The viscosity values must match the independent table-fit oracle within 1e-10. Default Auto execution does not assert forced-native execution.

Run exactly one test:

```sh
cargo test -p rumoca --features msl-sim-tests --test suite_core msl_glycol_viscosity_uses_its_checked_coefficient_interface -- --nocapture
```

The mandatory workspace test owner enables `rumoca/msl-sim-tests`; the existing CI job stages the pinned Modelica dependencies before that owner runs. Missing MSL fails rather than skipping this test.

The oracle uses the nine unchanged Glycol47 temperature/viscosity pairs in `Media/Incompressible.mo`. It fits log(viscosity) to `{1/T^2, 1/T, 1}` with Decimal precision 70, solving the three normal equations by elimination, then evaluates exp(polynomial) at 243.15 plus each sample time. The independent coefficients are 1249407.090549722967428722268629, -4756.243495674462988731728136 and -3.366009566358448237684913256. The expected values are 0.16625501162196409406, 0.16617741125690595139 and 0.16609985326956157769. These are the fitted curve, rather than the unfit first table sample 0.160.

Retained-wheel probes use compiler source `92f9169585948afafe8042ecb28b6c2763994930`, not the current compiler. The unchanged original TestGlycol compiled but simulation failed at continuous algebraic row 2 with `InvalidCallInterface`. Among seven independent public property probes, all compiled; viscosity alone failed simulation, while the six other properties completed three samples. Separate direct/nested exponential and raw/log-fitting controls all completed. Direct variable and constant vector Celsius conversion completed, while a Real-input function containing the viscosity table, mapped conversion in its constant initializer, fitting and exponential failed at Solve lowering. This narrows the source hypothesis to the composed initialization/call environment; it does not prove a particular producer or a current-source defect.

The DAE storage retained outside the repository shows that the mapped conversion and fitting calls remain in that function's coefficient expression. The generic phrase “cannot resolve its derivative reads” wraps the scalar lowering result and therefore does not establish that differentiation is the first failure. Current-source qualification must precede any additional semantic repair. Governing contracts are SPEC_0007, SPEC_0022 function/array rules, SPEC_0029 and SPEC_0033.

Raw bounded logs and guardian reports remain under the owned attempt directory named in the adjacent JSON digest receipt. A prior DAE-inspection attempt stopped at the scratch floor before model output; its retry completed. No current-source simulation pass, OMC parity, forced-native execution or complete MSL cohort result is claimed.

Current-source BEFORE subsequently executed with the ROOT-sealed CLI built from `c85128c50cd6e65198a712578c80679afc01aeaf`, binary SHA256 `e03b4f4093e4d66aadf57757e791b4959ad559222515e0fc1e98972b25f34f8b`. Both the unchanged original and public viscosity probe compiled through DAE, then failed Solve lowering with EL005/InvalidCallInterface at continuous algebraic row 2. Both processes exited 1, and zero CSV artifacts were emitted. This confirms that the current combined scope/identity fixes alone do not close this failure. The ordinary Rust test and any producer repair remain unrun. The adjacent `glycol-current-before.json` binds commands, logs and binary/source seals and reports the diagnostic resource-monitor scope explicitly.
# Actual temporary diagnostic at 3475

The tracing CLI compiled successfully. The first probe driver refused its
artifact before execution because it expected optimization level zero; the
workspace dev profile uses level one. A separate corrected driver verified
the actual artifact and sealed a copy outside Cargo's cache.

The corrected viscosity probe compiled through DAE, then failed Solve lowering
with the same row-2 `InvalidCallInterface`. All nine primal typed owners were
issued; neither owner-body nor nested `builder.call` failure callback fired.
This rules out those construction sites as the observed first failure, but
does not establish derivative availability or assertion replay correctness.
The probe produced no CSV. Exact source, binary, commands, guards, and raw
artifact hashes are bound in [glycol-diagnostic-before.json](glycol-diagnostic-before.json).
The temporary source patch must not be landed.
# Actual first rejection at 9188

The first scalar-context diagnostic failed to compile because temporary
logging formatted a view without `Debug`; its runtime probe was unrun. A
separate corrected diagnostic compiled and reproduced the lowering failure.
Its first rejection is `ExactExpressionForm::try_admit`, at Integer equality
expression 1576 in `dgelsy_vec`, with four active function frames. The source
span is 207504–213334 in the pinned MSL `Matrices.mo`.

This owner admits only Integer quotients or comparisons, so this is a
contextual Integer-expression boundary. All nine primal owners were issued
successfully before it. The enclosing row's “derivative reads” wording does
not identify the first rejecting operation. No CSV was produced. Commands,
source/binary identity, first-error lines, guards, and artifact hashes are in
[glycol-context-diagnostic-before.json](glycol-context-diagnostic-before.json).
The temporary diagnostic must not be landed; the repair remains unqualified.
