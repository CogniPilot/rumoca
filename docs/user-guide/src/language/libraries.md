# Using Modelica Libraries

Real models build on libraries — most importantly the
[Modelica Standard Library](https://github.com/modelica/ModelicaStandardLibrary)
(MSL). This chapter explains how Rumoca finds library code.

## Source Roots

A *source root* is a file or directory added to the compiler's search path.
When a model references `Modelica.Blocks.Continuous.PID`, Rumoca resolves
the name through the source roots you configured.

On the command line, pass `--source-root` (repeatable):

```bash
rumoca sim my_model.mo \
  --model MyPackage.MyModel \
  --source-root target/msl/ModelicaStandardLibrary-4.1.0/Modelica\ 4.1.0 \
  --source-root helper.mo
```

In a `rum.toml` scenario, use the top-level `source_roots` key with paths
relative to the scenario file:

```toml
source_roots = ["../modelica_libraries"]
```

In VS Code, set `rumoca.sourceRootPaths` (see
[VS Code Extension](../tools/vscode.md)).

## MODELICAPATH

Rumoca also honors the standard `MODELICAPATH` environment variable.
Entries (`:`-separated) are appended after explicit `--source-root` flags,
so system-wide library installs resolve without per-command flags.

## Pinned Dependencies for the Repository Examples

The examples in the Rumoca repository use pinned library versions declared
in `examples/modelica_dependencies.toml`. Fetch them once:

```bash
cargo xtask repo modelica-deps ensure
```

This downloads MSL and the CogniPilot Modelica Models (CMM) into `target/`.
The repository's committed VS Code settings reference those directories with
workspace-relative paths, so simulation and completion work for every
contributor without machine-specific configuration.

## Packages and `within`

Library code is organized as Modelica packages, either as a single `.mo`
file or as a directory tree with `package.mo` files. The `within` clause at
the top of a library file records which package the file belongs to. When you
add a library's root directory as a source root, all of its classes become
resolvable by their fully qualified names.

## Practical Tips

- Prefer pinned library versions over whatever happens to be checked out;
  scenario files plus pinned paths give reproducible runs.
- Large library trees compile slower in the browser playground than
  natively; use the native CLI or VS Code for MSL-heavy work.
- If a name does not resolve, check
  [Troubleshooting](../troubleshooting.md) — the usual cause is a missing or
  wrong source root.
