# Command-Line Interface

The main binary is `rumoca`. Every subcommand supports `--help`; this page
is a guided reference of the surface you will actually use.

```text
rumoca <COMMAND>

Commands:
  compile      Compile a Modelica file
  sim          Compile and simulate a model/scenario (subcommands: check, init, bench)
  fmt          Format Modelica files
  lint         Lint Modelica files
  completions  Print shell completion scripts
  targets      List built-in code generation targets and their capabilities
  cache        Inspect or prune the shared Rumoca cache
```

A global `--cache-dir <DIR>` overrides the on-disk cache root for any
subcommand.

## `rumoca sim`

Direct model run:

```bash
rumoca sim path/to/Model.mo --model Package.Model --t-end 10
```

Scenario run:

```bash
rumoca sim -c path/to/rumoca-scenario.toml
```

| Option | Meaning |
|---|---|
| `-c, --config <CONFIG>` | Run a `rumoca-scenario.toml` scenario instead of a direct sim |
| `-m, --model <MODEL>` | Main model/class to compile (auto-inferred when omitted) |
| `--source-root <PATH>` | Add a source root (repeatable); `MODELICAPATH` entries are appended after these |
| `--solver <SOLVER>` | `auto`, `bdf`, `esdirk34`, `trbdf2`, or `rk-like` — see [Solvers and Accuracy](../simulation/solvers.md) |
| `--t-end <T_END>` | End time. Direct runs default to 1.0; scenario runs use `[sim].t_end` |
| `--dt <DT>` | Optional fixed output interval; chosen automatically if omitted |
| `-o, --output <OUTPUT>` | Simulation report path (default `<MODEL>_results.html`) |
| `--inspect <MODE>` | Analyze instead of simulating: `structure`, `eval`, `jacobian` |
| `--at <NAME=VALUE,...@T>` | Evaluation point for `--inspect eval\|jacobian` |

Subcommands:

| Subcommand | Purpose |
|---|---|
| `rumoca sim check -c rumoca-scenario.toml` | Validate a scenario file without running it |
| `rumoca sim init` | Print a fully commented `rumoca-scenario.toml` starter template |
| `rumoca sim bench` | Benchmark compile, preparation, and hot simulation throughput |

Interactive viewer ports and scene files come from the `[transport.http]`
section of the scenario, not CLI flags.

## `rumoca compile`

`compile` separates three concerns into three flags:

- **`--emit <stage>-mo|<stage>-json`** dumps an intermediate representation.
  Stages are `ast`, `flat`, `dae`, `solve` (`solve` has no Modelica form, so
  only `solve-json` exists).
- **`--target <id|dir|file.jinja>`** runs code generation: a built-in target
  id (see `rumoca targets`), a directory containing `target.toml`, or a raw
  Jinja template.
- **`--phase <ast|flat|dae|solve>`** picks which IR a *raw* `.jinja`
  template receives (default `dae`). Ignored for built-in and directory
  targets, which declare their own IR.

```bash
rumoca compile Model.mo --emit dae-mo                 # DAE as Modelica, to stdout
rumoca compile Model.mo --emit solve-json -o out.json # solver IR as JSON
rumoca compile Model.mo --target sympy -o out/        # built-in target
rumoca compile Model.mo --target my.jinja --phase flat
```

`compile` shares `--model`, `--source-root`, `--inspect`, and `--at` with
`sim`, and adds `-v/--verbose` for friendly per-phase progress lines.

## `rumoca fmt` and `rumoca lint`

See [Formatter and Linter](./fmt-lint.md).

## `rumoca targets`

Prints the built-in code generation targets with their consumed IR stage,
generation mode, deployment class, readiness level, and per-feature support
columns, including native/scalar tensor support such as `matmul`, `linsolve`,
`elem`, and `stencil`. See [Targets and Templates](../codegen/targets.md).

## `rumoca cache`

Compilation artifacts are cached under the platform cache directory.

```bash
rumoca cache status          # size and entry counts
rumoca cache prune           # remove oldest cache files until under a size limit
```

Both accept `--root` (or the global `--cache-dir`) to operate on a
non-default location.

## `rumoca completions`

```bash
rumoca completions bash   # or zsh, fish, ...
```

## Environment

- `MODELICAPATH` — `:`-separated library roots, appended after explicit
  `--source-root` flags.

Rumoca deliberately has **no** behavior-changing `RUMOCA_*` environment
variables: every knob is a documented CLI flag or scenario key.
