# Web Playground

The browser playground runs the Rumoca compiler in WebAssembly. It is useful
for small models, quick experiments, and demos.

Public deployment:

```text
https://cognipilot.github.io/rumoca/
```

## Book Examples

The books should use focused WASM components rather than embedding the full
playground workbench. A good example page can host a single Monaco editor,
load a small in-page file set, call the Rumoca WASM compiler/simulator on those
files, and render only the controls, plots, or 3D view needed for that example.

That keeps the example native to the book page while reusing the same WASM
package as the playground. The missing piece is a small book-facing component
API for source files, model name, `rum.toml` scenario, and view selection.

The user and developer books are deployed beside it:

```text
https://cognipilot.github.io/rumoca/user-guide/
https://cognipilot.github.io/rumoca/dev-guide/
```

Limitations:

- Large package trees are slower than native builds.
- Browser storage and worker memory limits matter for full MSL-sized projects.
- Native interactive examples may have more solver/backend options than the
  browser build.

For larger models or external package development, prefer the native CLI or VS
Code extension.
