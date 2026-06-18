# Quick Start

This page assumes `rumoca` is on your `PATH`
([Installation](./installation.md)). If you are working from a source
checkout, replace `rumoca` with `cargo run -p rumoca --release --`.

## Simulate a Model Directly

Save this as `Ball.mo`:

```modelica
model Ball
  Real x(start=10);
  Real v(start=1);
  parameter Real g = 9.81;
equation
  der(x) = v;
  der(v) = -g;
  when x < 0 then
    reinit(v, -0.8*pre(v));
  end when;
end Ball;
```

Then run:

```bash
rumoca sim Ball.mo --t-end 10
```

Rumoca compiles the model, simulates to `t = 10 s`, and writes an HTML report
(`Ball_results.html` by default) with interactive plots of every variable.
The model name is inferred from the file; pass `--model` to pick a specific
class.

Useful options:

```bash
rumoca sim Ball.mo --model Ball --t-end 10 --solver rk-like --dt 0.01 -o ball.html
```

Use `--source-root` for packages that are not in the same source tree:

```bash
rumoca sim my_model.mo \
  --model MyPackage.MyModel \
  --source-root target/msl/ModelicaStandardLibrary-4.1.0
```

## Run a Scenario

The preferred repeatable workflow is a colocated `rumoca-scenario.toml` scenario file
that records the model, solver, plots, and viewer settings in one place:

```bash
rumoca sim -c examples/simulation/rumoca-scenario.ball.toml
```

Interactive examples use the same command shape:

```bash
rumoca sim -c examples/interactive/quadrotor/rumoca-scenario.acro.toml
```

Generate a commented starter scenario and validate it without running:

```bash
rumoca sim init > rumoca-scenario.toml
rumoca sim check -c rumoca-scenario.toml
```

See [Scenario Files](../simulation/scenario-tomls.md) for the full format.

## Compile or Generate Code

List the built-in code generation targets:

```bash
rumoca targets
```

Render a target:

```bash
rumoca compile examples/models/SympyDecay.mo \
  --model SympyDecay \
  --target sympy \
  --output /tmp/sympy_decay
```

Dump an intermediate representation of the compiler instead:

```bash
rumoca compile Ball.mo --emit dae-mo     # the DAE as Modelica source
rumoca compile Ball.mo --emit solve-json # the solver IR as JSON
```

## Next Steps

- Build a model from scratch in [Your First Model](./first-model.md).
- Learn the scenario format in
  [Scenario Files](../simulation/scenario-tomls.md).
- Explore the full [CLI reference](../tools/cli.md).
