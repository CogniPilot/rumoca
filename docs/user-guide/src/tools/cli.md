# CLI Reference

The main binary is `rumoca`.

```bash
rumoca <COMMAND>
```

Common commands:

| Command | Purpose |
|---|---|
| `compile` | Compile a Modelica model to an IR or target output |
| `sim` | Simulate directly or run a `rum.toml` scenario |
| `check` | Compile and print balance/summary information |
| `fmt` | Format Modelica files |
| `lint` | Lint Modelica files |
| `targets` | List built-in code generation targets and capabilities |
| `cache` | Inspect or prune the shared Rumoca cache |

## `rumoca sim`

Direct model run:

```bash
rumoca sim path/to/Model.mo --model Package.Model --t-end 10
```

Scenario run:

```bash
rumoca sim -c path/to/rum.toml
```

Useful options:

| Option | Meaning |
|---|---|
| `--model` | Override the model name |
| `--source-root` | Add a source root for direct runs |
| `--solver` | Choose `auto`, `bdf`, or `rk-like` |
| `--t-end` | Simulation end time |
| `--dt` | Output or fixed step interval, depending on runner path |
| `--inspect` | Inspect the lowered model (`structure`, `eval`, `jacobian`); see `--at` |

Interactive viewer ports/scene come from the `[transport.http]` section of the
scenario's `rum.toml`, not CLI flags.

`compile` separates three concerns into three flags:

Dump an IR stage with `--emit <stage>-mo` (Modelica) or `--emit <stage>-json`
(JSON); stage is `ast`/`flat`/`dae`/`solve` (no `solve-mo` — the solver IR has
no Modelica form):

```bash
rumoca compile path/to/Model.mo --model Package.Model --emit dae-mo      # DAE IR as Modelica
rumoca compile path/to/Model.mo --model Package.Model --emit solve-json  # solver IR as JSON
```

Generate code with `--target` (a built-in target id, a directory containing
`target.toml`, or a raw `.jinja` template). `--phase` is only used with a raw
`.jinja` target, to pick which IR the template receives (default `dae`):

```bash
rumoca compile path/to/Model.mo --model Package.Model --target sympy -o out
rumoca compile path/to/Model.mo --model Package.Model --target my.jinja --phase flat
```

Run `rumoca targets` to list the built-in targets and their capabilities.
