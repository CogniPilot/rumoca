# Your First Model

This tutorial builds a small physical model from scratch and explains each
piece of Modelica syntax as it appears. The code blocks are live: you can
edit and simulate them directly on this page.

## A Cooling Cup of Coffee

Newton's law of cooling says the temperature `T` of an object approaches the
ambient temperature `T_amb` at a rate proportional to their difference:

```modelica,interactive
model Coffee "Newton's law of cooling"
  parameter Real T_amb = 20.0 "Ambient temperature [degC]";
  parameter Real tau = 300.0 "Cooling time constant [s]";
  Real T(start = 90.0) "Coffee temperature [degC]";
equation
  der(T) = (T_amb - T) / tau;
  annotation(experiment(StopTime = 1800.0));
end Coffee;
```

Press **▶ Simulate** and watch the temperature decay toward 20 °C.

Reading the model line by line:

- `model Coffee "..."` — declares a class named `Coffee`. The string after
  the name is a description; tools display it but it has no effect on the
  equations.
- `parameter Real T_amb = 20.0` — a *parameter* is fixed during a simulation
  but adjustable between runs. Try changing it and re-running.
- `Real T(start = 90.0)` — a continuous variable. `start` gives its initial
  value. Because `der(T)` appears in an equation, `T` is a *state*: the
  solver integrates it through time.
- `der(T) = (T_amb - T) / tau` — an *equation*, not an assignment. You could
  equally write `(T_amb - T) / tau = der(T)`; the compiler decides how to
  solve the system.
- `annotation(experiment(StopTime = 1800.0))` — the standard Modelica way to
  store default simulation settings with the model. The live editors on
  these pages and the web playground honor it; native CLI runs currently use
  `--t-end` (default 1.0 s) or the `[sim]` section of a scenario file
  instead.

## Adding a Second State

Models grow by adding variables and equations — one equation per unknown.
Here is the same idea applied to a mass hanging on a spring, which needs two
states (position and velocity):

```modelica,interactive
model HangingMass
  parameter Real m = 0.5 "Mass [kg]";
  parameter Real k = 20.0 "Spring constant [N/m]";
  parameter Real c = 0.5 "Damping [N.s/m]";
  parameter Real g = 9.81;
  Real x(start = 0.0) "Displacement below natural length [m]";
  Real v(start = 0.0);
  Real f_spring "Spring force [N]";
equation
  f_spring = -k * x;
  der(x) = v;
  m * der(v) = f_spring - c * v + m * g;
  annotation(experiment(StopTime = 5.0));
end HangingMass;
```

`f_spring` is an *algebraic* variable: it has no `der()`, so the compiler
solves it from its equation at every step instead of integrating it. Mixing
differential and algebraic equations like this is what makes the system a
DAE (differential-algebraic equation system) — press **Show DAE** to see the
sorted system Rumoca produces.

## Running It Natively

Save the model as `HangingMass.mo` and run:

```bash
rumoca sim HangingMass.mo --t-end 5
```

The HTML report `HangingMass_results.html` plots all variables.

## Recording the Run as a Scenario

Once a model has settings worth keeping — solver, plots, output paths —
record them in a `rumoca-scenario.toml` scenario next to the model:

```toml
[rumoca]
version = "1"
task = "simulate"

[model]
file = "HangingMass.mo"
name = "HangingMass"

[sim]
t_end = 5.0
solver = "auto"
output = "hanging_mass.html"
```

```bash
rumoca sim -c rumoca-scenario.toml
```

From here:

- [Modeling with Equations](../language/overview.md) explains the language
  concepts systematically.
- [Events and Discrete Behavior](../language/events.md) adds switching,
  impacts, and sampled control.
- [Scenario Files](../simulation/scenario-tomls.md) documents everything a
  `rumoca-scenario.toml` can do.
