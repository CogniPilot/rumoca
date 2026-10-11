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
