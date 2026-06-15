# Events and Discrete Behavior

Physical models often mix continuous dynamics with discrete switching:
impacts, controllers that sample, valves that open. Modelica expresses these
with *events*, and the solver locates them precisely instead of stepping
blindly past them.

## When Equations and reinit

A `when` equation activates at the instant its condition becomes true. The
classic example is the bouncing ball, which reverses its velocity at impact:

```modelica,interactive
model Ball
  Real x(start = 10) "Height [m]";
  Real v(start = 1) "Velocity [m/s]";
  parameter Real g = 9.81;
  parameter Real e = 0.8 "Coefficient of restitution";
equation
  der(x) = v;
  der(v) = -g;
  when x < 0 then
    reinit(v, -e * pre(v));
  end when;
  annotation(experiment(StopTime = 10.0));
end Ball;
```

- `when x < 0 then ... end when` — fires once at the crossing instant, not
  continuously while the condition holds.
- `pre(v)` — the value of `v` immediately *before* the event.
- `reinit(v, ...)` — restarts the state `v` from a new value.

The solver detects the zero crossing of `x`, locates the impact time, applies
the `reinit`, and continues. Run the example and zoom in on the velocity
trace: each bounce is a clean jump, not a smoothed-over spike.

## Sampled (Clocked-Style) Control

`sample(start, period)` fires periodically, which is the standard way to model
a digital controller around a continuous plant:

```modelica,interactive
model SampledControl "Continuous plant with a sampled P controller"
  parameter Real kp = 2.0 "Proportional gain";
  parameter Real Ts = 0.2 "Sample period [s]";
  parameter Real r = 1.0 "Reference";
  Real x(start = 0.0) "Plant state";
  discrete Real u(start = 0.0) "Held control signal";
equation
  der(x) = -x + u;
  when sample(0, Ts) then
    u = kp * (r - pre(x));
  end when;
  annotation(experiment(StopTime = 6.0));
end SampledControl;
```

`u` is `discrete`: it changes only when the `when` fires and is held constant
(zero-order hold) in between. Try shortening `Ts` toward continuous control,
or raising `kp` until the loop rings.

## Hysteresis with elsewhen

`elsewhen` chains mutually exclusive switching conditions. A thermostat with
a hysteresis band:

```modelica,interactive
model Thermostat "Bang-bang temperature control with hysteresis"
  parameter Real T_set = 21.0 "Setpoint [degC]";
  parameter Real band = 1.0 "Hysteresis half-width [degC]";
  parameter Real T_amb = 5.0 "Outside temperature [degC]";
  parameter Real tau = 600.0 "Thermal time constant [s]";
  parameter Real heat = 0.02 "Heater authority [degC/s]";
  Real T(start = 15.0) "Room temperature [degC]";
  Boolean on(start = true) "Heater state";
equation
  der(T) = (T_amb - T) / tau + (if on then heat else 0.0);
  when T > T_set + band then
    on = false;
  elsewhen T < T_set - band then
    on = true;
  end when;
  annotation(experiment(StopTime = 7200.0, Solver = "rk-like"));
end Thermostat;
```

This model pins `Solver = "rk-like"` in its experiment annotation: the
explicit solver handles its rapid relay switching robustly, while the default
implicit solver can currently stall on it (see
[Solvers and Accuracy](../simulation/solvers.md)).

## Conditional Expressions vs Events

An `if`-expression inside an equation (`if on then heat else 0.0`) also
generates events at its switch points so the integrator never smears across
a discontinuity. When a discontinuity is harmless and you want to suppress
event handling, Modelica provides `noEvent(...)`; `smooth(...)` asserts
differentiability.

## Things to Keep In Mind

- `when` bodies relate *discrete* values; use `reinit` to restart continuous
  states.
- `pre(x)` is only meaningful for discrete-valued variables or at event
  instants.
- Event-heavy models simulate best with explicit solvers today; pin
  `Solver = "rk-like"` in the experiment annotation or pass
  `--solver rk-like`.
