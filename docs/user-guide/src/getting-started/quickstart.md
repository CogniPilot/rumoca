# Quick Start

## Direct Simulation

For a standalone Modelica file, call `rumoca sim` directly:

```bash
cargo run -p rumoca -- \
  sim examples/models/Ball.mo \
  --model Ball \
  --t-end 10 \
  --solver rk-like
```

Use `--source-root` for packages that are not in the same source tree:

```bash
cargo run -p rumoca -- \
  sim my_model.mo \
  --model MyPackage.MyModel \
  --source-root target/msl/ModelicaStandardLibrary-4.1.0/Modelica\ 4.1.0
```

## Scenario Simulation

The preferred repeatable workflow is a colocated `rum.toml` scenario:

```bash
cargo run -p rumoca --release -- \
  sim -c examples/simulation/rum.ball.toml
```

Interactive examples use the same command shape:

```bash
cargo run -p rumoca --release -- \
  sim -c examples/interactive/quadrotor/rum.acro.toml
```

## Check a Scenario

Validate a scenario before running it:

```bash
cargo run -p rumoca -- sim check -c examples/interactive/quadrotor/rum.acro.toml
```

## Compile or Generate Code

List available targets:

```bash
cargo run -p rumoca -- targets
```

Render a target:

```bash
cargo run -p rumoca -- \
  compile examples/models/SympyDecay.mo \
  --model SympyDecay \
  --target sympy \
  --output /tmp/sympy_decay
```
