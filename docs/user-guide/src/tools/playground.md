# Web Playground

The browser playground runs the full Rumoca compiler in WebAssembly: a
project file tree, Monaco editors with LSP support, simulation with plots,
code generation, and package archive loading — no install required.

```text
https://cognipilot.github.io/rumoca/
```

It is useful for small models, quick experiments, sharing reproductions in
bug reports, and demos.

## Live Examples in This Book

The runnable code blocks throughout this book (look for the **▶ Simulate**
button) use the same WASM package as the playground, embedded as focused
mini editors:

- the same compiler, solvers, and diagnostics as the native CLI,
- Monaco-based editing with Rumoca's completion, hover, and error checking,
- inline plots, DAE views, and per-example visualizations.

The first run on a page downloads the WASM compiler; afterwards it is
cached by the browser. Models honor their `experiment` annotation
(`StopTime`, `Interval`, `Tolerance`, `Solver`).

## Limitations

- Large package trees compile more slowly than native builds.
- Browser storage and worker memory limits matter for full MSL-sized
  projects.
- Native interactive examples may have more solver/backend options than the
  browser build.

For larger models or external package development, prefer the native CLI or
the VS Code extension.
