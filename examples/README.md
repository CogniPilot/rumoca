# Rumoca Examples

Examples are organized by workflow:

- `models/`: shared Modelica models used by multiple examples.
- `simulation/`: batch simulation scenarios that produce plots or reports.
- `interactive/`: browser or input-driven simulations with scene assets.
- `codegen/`: code generation scenarios and custom target bundles.
- `codegen/custom_casadi.jinja`: direct raw-template codegen example.

Generated code from `codegen/` scenarios goes under `codegen/gen/`, which is
ignored by git.

## Modelica Dependencies

Pinned external Modelica packages are listed in `modelica_dependencies.toml`.
The MSL-backed examples use `ModelicaStandardLibrary`; the quadrotor examples
use CogniPilot Modelica Models.

The easiest setup is:

```bash
cargo xtask repo modelica-deps ensure
```

That downloads the pinned archives into `target/msl/` and `target/cmm/`, which
matches the committed example `source_roots`.

Without `rum`, download the same packages yourself, then either:

- Put their package roots in the `rumoca-scenario.toml` scenario `source_roots`.
- Add their package roots to `MODELICAPATH`.

Scenario `source_roots` are checked before `MODELICAPATH`, so a scenario can
pin a dependency path even when your shell has a broader Modelica path.
