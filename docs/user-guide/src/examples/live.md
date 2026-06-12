# Live Examples

Every block on this page runs the real Rumoca compiler in your browser.
Edit freely — the editors have Rumoca's completion, hover, and error
checking — then **▶ Simulate** to integrate and plot, or **Show DAE** to see
the compiled equation system. **Reset** restores the original text.

More live examples appear throughout the book:
[spring-mass-damper](../introduction.md),
[cooling coffee and a hanging mass](../getting-started/first-model.md),
[events and discrete behavior](../language/events.md),
[the stiff Van der Pol](../simulation/solvers.md), and the
[turkey PDE with an animated cross-section](../language/arrays-pde.md).

## Exponential Decay

The simplest possible ODE — one state, one parameter:

```modelica,interactive
model SympyDecay
  Real x(start = 1);
  parameter Real k = 0.5;
equation
  der(x) = -k * x;
  annotation(experiment(StopTime = 10.0));
end SympyDecay;
```

## Bouncing Ball

Events: a zero crossing detected by the solver, with a state jump:

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

Try `e = 1.0` (elastic) or `e = 0.5` (dead tennis ball).

## Coupled Oscillators

Two masses, three springs — energy sloshes between the modes:

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

The beat period is set by the coupling strength — weaken `kc` and watch the
energy exchange slow down.

## Lotka–Volterra Predator–Prey

A classic nonlinear system with closed orbits:

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

## Browser Notes

The first **▶ Simulate** on a page downloads the WASM compiler (cached
afterwards). Models honor their `experiment` annotation — `StopTime`,
`Interval`, `Tolerance`, and `Solver`. For bigger work, use the
[Web Playground](../tools/playground.md) or the native tools.
