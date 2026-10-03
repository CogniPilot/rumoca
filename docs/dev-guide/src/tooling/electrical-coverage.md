# Electrical coverage work

Reviewed reference boundaries for `Modelica.Electrical.Analog` and
`Modelica.Electrical.PowerConverters` parity. A boundary here grants no
strict-high or certification credit; the model stays in every cohort sweep.

## AmplifierWithOpAmpDetailed has no converged OMC reference

Against the default OMC reference the Rumoca trace compares 66 channels: 24
high, 31 minor, and 11 deviating (maximum bounded normalized L1 0.221). The
deviating channels are the op-amp's internal states. Between 0.9 ms and 1.4 ms
the default OMC trace makes a single non-periodic excursion that the periodic
Rumoca trace does not make; outside that window the traces agree.

OMC cannot refine its own reference. The same model, simulated by the same OMC
build with only the tolerance tightened, fails partway through the run:

```modelica
loadModel(Modelica, {"4.1.0"}); getErrorString();
simulate(Modelica.Electrical.Analog.Examples.AmplifierWithOpAmpDetailed,
  tolerance=1e-10, numberOfIntervals=12, outputFormat="csv",
  variableFilter="time|opAmp.v_in|opAmp.v_source|opAmp.q_fp1",
  fileNamePrefix="amp_tight"); getErrorString();
simulate(Modelica.Electrical.Analog.Examples.AmplifierWithOpAmpDetailed,
  numberOfIntervals=12, outputFormat="csv",
  variableFilter="time|opAmp.v_in|opAmp.v_source|opAmp.q_fp1",
  fileNamePrefix="amp_default"); getErrorString();
```

Tolerance 1e-10 (`method = 'dassl'`, stop time 0.003 s):

```text
messages = "Simulation execution failed for model: Modelica.Electrical.Analog.Examples.AmplifierWithOpAmpDetailed
DASKR--  NONLINEAR SOLVER FAILED TO CONVERGE
LOG_STDOUT        | info    | model terminate | Integrator failed. | Simulation terminated at time 0.000136928
```

Default tolerance 2e-7:

```text
LOG_SUCCESS       | info    | The initialization finished successfully without homotopy method.
LOG_SUCCESS       | info    | The simulation finished successfully.
```

The only OMC trace that exists is therefore the one with the excursion, and no
tighter OMC run can confirm or refute it. The comparison is not identifying,
so the model carries a `reference_failure` row in
`crates/rumoca-test-msl/tests/msl_tests/msl_trace_compare_exclusions.json`.
Official reference traces and tolerances are unchanged.

## OMC folds `atan2(0, x)` to 0

`Modelica.ComplexMath.arg(c)` is `atan2(c.im, c.re)`, which is pi for a negative
real number. The OMC build that produces the references returns 0 instead
whenever the imaginary part is structurally zero, whatever the sign of the real
part:

```modelica
model ARG2
  Real x = sin(10*time);
  Real zero = 0;
  Real a = atan2(zero, x);
  Complex c = Complex(x, 0);
  Real b = Modelica.ComplexMath.arg(c);
end ARG2;
```

```modelica
loadModel(Modelica, {"4.1.0"}); getErrorString();
loadFile("ARG2.mo"); getErrorString();
simulate(ARG2, stopTime=1, numberOfIntervals=10, outputFormat="csv"); getErrorString();
```

OMC (`openmodelica-unstable-2026-07-21`), columns `time, x, a, b`:

```text
0.4  -0.7568024953079282  0  0
0.5  -0.9589242746631385  0  0
0.6  -0.2794154981989259  0  0
1.0  -0.5440211108893698  0  0
```

Rumoca at the same samples returns `a = b = 3.141592653589793`, and 0 where
`x > 0`. An OMC channel computed this way is the constant 0, so it cannot
distinguish a correct angle from a wrong one.

Two FundamentalWave models have no deviating or minor channel other than such
an angle. Their phase-1 winding has orientation 0, so `V_m = (2/pi)*N*i` has a
structurally zero imaginary part while its real part follows the alternating
current: Rumoca's angle alternates between 0 and pi, OMC's is 0 throughout.

| Model | Channels (high / minor / deviating) | Deviating channels |
|---|---|---|
| `Magnetic.FundamentalWave.Examples.Components.EddyCurrentLosses` | 678 / 0 / 2 | `arg_V_m` of the phase-1 converter of `converter_m` and `converter_e` |
| `Magnetic.FundamentalWave.Examples.Components.SinglePhaseInductance` | 78 / 0 / 4 | `arg_Phi`, `arg_V_m` of `reluctance_m` and `converter_m` |

Each carries a `reference_failure` row in
`crates/rumoca-test-msl/tests/msl_tests/msl_trace_compare_exclusions.json`.

## Angles of zero and negative real phasors in QuasiStatic FluxTubes

In three QuasiStatic FluxTubes models every non-high channel is an `arg_*`
angle, and every difference is the sign of a zero, not a value:

- At `t = 0` a flux tube's phasor is exactly zero. `atan2` of a zero phasor
  returns 0, pi or -pi according to the signs of its two zeros (Rumoca
  evaluates `cuboidLeft.Phi` as `(-0, -0)`, hence -pi; OMC records 0). The
  angle of a zero phasor is undefined.
- A phasor with a negative real part and a zero imaginary part lies on the
  branch cut: `atan2(+0, x)` is pi and `atan2(-0, x)` is -pi, the same angle.
  `cuboidTop` and `idle` in CuboidSections, and both angle channels of
  CylinderLeakage, record pi in one trace and -pi in the other for the whole
  run.

| Model | Channels (high / minor / deviating) | Differences |
|---|---|---|
| `Magnetic.QuasiStatic.FluxTubes.Examples.FixedShapes.CuboidSections` | 171 / 0 / 11 | 11 angles at `t = 0`; 5 of them pi against -pi throughout |
| `Magnetic.QuasiStatic.FluxTubes.Examples.FixedShapes.CylinderSections` | 156 / 0 / 8 | 8 angles at `t = 0` only |
| `Magnetic.QuasiStatic.FluxTubes.Examples.Leakage.CylinderLeakage` | 232 / 0 / 2 | 2 angles, pi against -pi throughout |

Pointwise comparison of an angle is not identifying at a zero phasor or across
the branch cut, so each carries a `comparator_limitation` row in
`crates/rumoca-test-msl/tests/msl_tests/msl_trace_compare_exclusions.json`.
Official reference traces and tolerances are unchanged.

## Angles of zero and negative real phasors in Electrical.QuasiStatic

The same two cases account for every non-high channel of two
Electrical.QuasiStatic examples:

- In `Polyphase.Examples.BalancingStar` the load balances the source: the phase
  currents 10 A at 120 deg and 10/sqrt(3) A at -90 deg and -30 deg sum to an
  exactly zero neutral current. Both traces carry only floating-point residue
  (at most 4e-15 A) on `currentSensor0.i`, whose real and imaginary channels
  agree; `atan2` of that residue gives -2.678 rad in Rumoca and -2.761 rad in
  OMC.
- In `SinglePhase.Examples.Rectifier` the quasi-static current is exactly zero
  until the load ramp starts at `t = 0.1`, so its angle channels follow the
  signs of the zeros (-pi against 0, and 0 against -pi for the source current,
  with `voltageQS.pf = cos(arg_v - arg_i)` 1 against -1). After `t = 0.1` the
  load is resistive and `voltageQS.arg_i` lies on the branch cut, pi against
  -pi.

| Model | Channels (high / minor / deviating) | Differences |
|---|---|---|
| `Electrical.QuasiStatic.Polyphase.Examples.BalancingStar` | 1213 / 0 / 1 | angle of the zero neutral current throughout |
| `Electrical.QuasiStatic.SinglePhase.Examples.Rectifier` | 231 / 0 / 5 | 4 angles and 1 power factor for `t <= 0.1`; 1 angle pi against -pi after |

Each carries a `comparator_limitation` row retired by
`angle_branch_aware_comparison`.
