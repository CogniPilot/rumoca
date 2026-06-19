# Modeling with Equations

This chapter explains the core ideas of equation-based modeling as Rumoca
implements them. It is not a full Modelica tutorial — the
[Modelica Language Specification](https://specification.modelica.org/) is the
authority — but it covers what you need to write and read most models.

## Equations, Not Assignments

A Modelica `equation` declares a relationship that must hold at every instant.
The compiler — not you — decides which variable each equation solves for,
in what order, and which variables become integrator states. This is why you
can write `m * der(v) = f` instead of `der(v) := f / m`, and why models stay
readable as they grow: adding a component adds equations, and the compiler
re-sorts the whole system.

A model is *balanced* when it has exactly one equation per unknown. Rumoca
checks this during compilation and reports counts when they disagree.

## Variability

Every variable has a *variability* that tells the compiler how it can change:

| Declaration | Meaning |
|---|---|
| `constant Real c = 2.0` | Fixed forever, usable in types and array sizes |
| `parameter Real k = 1.0` | Fixed during a run, settable between runs |
| `discrete Real u` | Changes only at events, holds its value between them |
| `Real x` | Continuous-time variable |
| `input Real f` / `output Real y` | Causal connectors for the model boundary; interactive simulation routes signals to `input`s |

## States and Initial Values

A continuous variable that appears under `der(...)` becomes a *state*. Give
states an initial value with the `start` attribute:

```modelica
Real x(start = 1.0);
```

Variables without `der()` are *algebraic*: solved from the equation system
at each step rather than integrated.

## A Worked Example

The Van der Pol oscillator shows a nonlinear two-state system. The `mu`
parameter controls how strongly nonlinear (and numerically stiff) it is —
edit it and re-run:

```modelica,interactive
model VanDerPol "Van der Pol oscillator"
  parameter Real mu = 5.0 "Nonlinearity / stiffness";
  Real x(start = 2.0);
  Real y(start = 0.0);
equation
  der(x) = y;
  der(y) = mu * (1 - x^2) * y - x;
  annotation(experiment(StopTime = 30.0));
end VanDerPol;
```

The same equation style handles coupled systems. Here two masses exchange
energy through a weak coupling spring:

```modelica,interactive
model CoupledOscillators
  parameter Real m = 1.0;
  parameter Real k = 10.0 "Outer springs";
  parameter Real kc = 1.0 "Coupling spring";
  Real x1(start = 1.0);
  Real v1(start = 0.0);
  Real x2(start = 0.0);
  Real v2(start = 0.0);
equation
  der(x1) = v1;
  m * der(v1) = -k * x1 - kc * (x1 - x2);
  der(x2) = v2;
  m * der(v2) = -k * x2 - kc * (x2 - x1);
  annotation(experiment(StopTime = 30.0));
end CoupledOscillators;
```

And nonlinear equations do not need to be mechanical. This predator-prey
model has two interacting states and closed orbits:

```modelica,interactive
model LotkaVolterra
  parameter Real alpha = 1.1 "Prey growth";
  parameter Real beta = 0.4 "Predation";
  parameter Real delta = 0.1 "Predator efficiency";
  parameter Real gamma = 0.4 "Predator death";
  Real prey(start = 10.0);
  Real predator(start = 10.0);
equation
  der(prey) = alpha * prey - beta * prey * predator;
  der(predator) = delta * prey * predator - gamma * predator;
  annotation(experiment(StopTime = 50.0));
end LotkaVolterra;
```

## Structure: Models, Packages, Extends

Modelica organizes code in classes:

- `model` / `block` / `class` — components with equations.
- `package` — a namespace holding other classes, usually one per library.
- `record` — pure data structures.
- `function` — algorithmic code callable from equations.
- `connector` — interface definitions used by `connect(...)`.

Classes compose by *instantiation* (declaring a component of another class)
and *inheritance* (`extends`). Modifications customize an instance in place:

```modelica
model TwoTanks
  Tank tank1(area = 2.0);
  Tank tank2(area = 0.5, h(start = 1.0));
equation
  connect(tank1.outlet, tank2.inlet);
end TwoTanks;
```

`connect` generates equality equations for potential variables and sum-to-zero
equations for `flow` variables, which is how component-based physical
modeling works without manually wiring every force and current.

## What the Compiler Does With Your Equations

When you press run, Rumoca:

1. parses and resolves the model and everything it references,
2. type-checks and instantiates components with their modifications,
3. flattens the class hierarchy and `connect` sets into one equation system,
4. sorts and analyzes the system structurally (matching equations to
   unknowns, finding simultaneous blocks, tearing algebraic loops),
5. hands the prepared system to a numerical solver.

You can watch each stage with `rumoca compile --emit <stage>` and
`--inspect structure` — see
[Inspecting and Debugging Models](../simulation/inspect.md). The full
pipeline is documented for contributors in the
[Rumoca Dev Guide](https://cognipilot.github.io/rumoca/dev-guide/) book.
