# Web Playground

The browser playground runs the full Rumoca compiler in WebAssembly: a
project file tree, Monaco editors with LSP support, simulation with plots,
code generation, and package archive loading — no install required.

```text
https://cognipilot.github.io/rumoca/
```

It is useful for small models, quick experiments, sharing reproductions in
bug reports, and demos.

## Runnable Blocks in This Book

The runnable code blocks throughout this book (look for the **▶ Simulate**
button) use the same WASM package as the playground, embedded as focused
mini editors:

- the same compiler, solvers, and diagnostics as the native CLI,
- Monaco-based editing with Rumoca's completion, hover, and error checking,
- inline plots, DAE views, and per-example visualizations.

The first run on a page downloads the WASM compiler; afterwards it is
cached by the browser. Models honor their `experiment` annotation
(`StopTime`, `Interval`, `Tolerance`, `Solver`).

## Choosing Plot Channels

After a simulation completes, the results panel plots the channels selected by
the active view. Open the panel's visualization settings (the gear button) to
edit a `timeseries` view. Each view offers two ways to choose channels, and
they stay in sync:

- A **channel picker**: a scrollable checkbox list of every available channel,
  grouped into **States** and **Outputs** (outputs are the non-state
  algebraic variables). Toggling a checkbox adds or removes that exact channel
  from the plot.
- A free-text **Y** field for advanced entry, including the wildcards
  `*states` (all states), `*outputs` (all outputs), and `*all` (every
  channel). Wildcards can be mixed with explicit channel names.

The picker appears only once a run has produced channels; before then the Y
field still accepts names and wildcards. The same selection is stored per model
in the scenario file under `[[plot.views]]` (see
[Scenario TOMLs](../simulation/scenario-tomls.md)), so a saved view reopens with
the same channels on the next run.

## Limitations

- Large package trees compile more slowly than native builds.
- Browser storage and worker memory limits matter for full MSL-sized
  projects.
- Native interactive examples may have more solver/backend options than the
  browser build.

For larger models or external package development, prefer the native CLI or
the VS Code extension.
