# Solvers and Accuracy

Rumoca ships several integration methods behind one `--solver` flag (CLI),
`solver` key (`[sim]` in scenarios), or `Solver` experiment annotation.

## Choosing a Solver

| Solver | Kind | Use for |
|---|---|---|
| `auto` | — | Default; picks a method from the model's structure |
| `bdf` | Implicit multistep (diffsol) | Stiff systems, smooth DAEs |
| `esdirk34` | Implicit SDIRK tableau (diffsol) | Stiff DAEs, one-step alternative to BDF |
| `trbdf2` | Implicit SDIRK tableau (diffsol) | Stiff DAEs |
| `rk-like` | Explicit Runge–Kutta-style | Non-stiff systems, event-heavy models |

Rules of thumb:

- Start with `auto`.
- *Stiff* systems — fast and slow dynamics together (chemical kinetics,
  stiff mechanical contacts, `mu`-large Van der Pol) — need an implicit
  solver; explicit methods crawl with tiny steps.
- Models that switch rapidly (relays, hysteresis controllers) currently run
  most robustly with `rk-like`; the implicit path can stall with a
  *step size too small* error near dense event cascades. Pin it in the
  model: `annotation(experiment(Solver = "rk-like"))`.

## Tolerances and Output Interval

Implicit solvers control local error against relative/absolute tolerances.
Sources, in priority order:

1. CLI/scenario settings (`--dt`, `[sim] dt`).
2. The model's `experiment` annotation: `Tolerance` and `Interval` are used
   by scenario and browser runs.
3. Runtime defaults.

`--dt` (or `[sim] dt`) sets the *output* interval — and the fixed step for
the explicit runner path. If omitted, the runtime chooses automatically.

## Experiment Annotations

```modelica
annotation(experiment(
  StartTime = 0.0,
  StopTime = 30.0,
  Tolerance = 1e-6,
  Interval = 0.01,
  Solver = "rk-like"
));
```

`StopTime`, `StartTime`, `Tolerance`, `Interval`, and `Solver` (also
accepted: `Algorithm`, `__Dymola_Algorithm`) are parsed from the annotation.
Browser/live runs and the playground honor them fully; native *direct* CLI
runs currently take end time from `--t-end` (default 1.0 s) and scenario
runs from `[sim]`.

## Events and the Solver

All solvers cooperate with the event system: zero crossings from `when`
conditions and if-expressions are located, the state is re-initialized
(`reinit`), and integration restarts cleanly at the event instant. Sampled
events (`sample(t0, period)`) are scheduled exactly, not detected.

## Try It

The Van der Pol oscillator becomes stiff as `mu` grows. Compare solvers by
editing the annotation — try `mu = 1000` with `Solver = "bdf"` versus
`Solver = "rk-like"` and watch the run time in the status line:

```modelica,interactive
model VanDerPolStiff
  parameter Real mu = 1000.0 "Try 1, 5, 1000";
  Real x(start = 2.0);
  Real y(start = 0.0);
equation
  der(x) = y;
  der(y) = mu * (1 - x^2) * y - x;
  annotation(experiment(StopTime = 3000.0, Solver = "bdf"));
end VanDerPolStiff;
```
