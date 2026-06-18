# Docs and Pages

## Site Layout

GitHub Pages serves one artifact assembled by the WASM build job in CI:

```text
https://cognipilot.github.io/rumoca/            ← playground (packages/playground)
https://cognipilot.github.io/rumoca/user-guide/ ← mdBook, docs/user-guide
https://cognipilot.github.io/rumoca/dev-guide/  ← mdBook, docs/dev-guide
https://cognipilot.github.io/rumoca/pkg/<subdir>/ ← rumoca-bind-wasm package
https://cognipilot.github.io/rumoca/src/        ← playground JS modules
```

The deploy job publishes whatever the WASM build job staged into
`gh-pages/`; any new public static content must be copied there in that CI
step (`.github/workflows/ci.yml`, "Prepare GitHub Pages content").

## Books

Both books are mdBook projects (`docs/user-guide`, `docs/dev-guide`).

```bash
cargo xtask docs build       # build both books
cargo xtask docs serve       # build and host both books locally
cargo xtask verify docs        # the docs CI gate
```

## Repository Web Layout

| Location | Role |
|---|---|
| `packages/rumoca` | npm/WASM package entrypoint. Its build writes generated `dist/<profile>-<variant>` packages; checked-in sources stay small. |
| `packages/rumoca-web` | Single source for hand-written browser runtime and visualization JavaScript/CSS, plus npm-managed browser dependencies. |
| `crates/rumoca-web` | Rust crate that embeds the web package for native reports/viewer shells; `crates/rumoca-web/web` points at `packages/rumoca-web` so Cargo packaging and npm packaging use the same sources. |
| `packages/playground` | Static playground app and browser smoke tests; it is a product package, not a reusable JavaScript library. |
| `packages/vscode` | VS Code extension, including offline webview assets built from the same `packages/rumoca-web` source package. |
| `docs/user-guide/live` | mdBook live-example runner. `docs/dev-guide/live` is a symlink to it. |
| `infra/` | Installer, Docker, and Nix integration; not npm/Cargo package source. |

Do not check generated/minified vendor JavaScript into the tree. Browser
dependencies are normal npm dependencies and are staged into ignored/generated
outputs by package-local builds or `cargo xtask` commands.

## Live Examples

Fenced blocks annotated `modelica,interactive` become editable, runnable
mini editors backed by the WASM package. The runner is
`docs/user-guide/live/rumoca-live.js` (+ `.css`), wired into both books via
`additional-js`/`additional-css` in their `book.toml`s —
`docs/dev-guide/live` is a symlink to the user-guide copy so there is a
single source.

How it works:

- **Editors** are Monaco from staged local vendor assets, using the shared
  Modelica language definition
  `packages/rumoca-web/runtime/modelica_language.js`, with a plain-textarea
  fallback when those assets are absent.
- **Language services** (completion, hover, diagnostics-as-markers) call
  the `lsp_*` functions of `rumoca-bind-wasm` directly on the main thread —
  book examples are small, so no worker is needed.
- **Simulate** calls the shared `packages/rumoca-web/runtime/rumoca_runtime.js`
  interface, which loads the parsed source-root cache and routes explicit BDF
  requests through the lazy diffsol addon. Results render as an inline SVG
  plot. **Show DAE** uses the same runtime to render the `dae-modelica`
  target.
- **Scenario blocks** can point at a staged `rumoca-scenario.toml` with
  `// rumoca-live-scenario: ...`. The book build copies repository examples
  into `repo-examples/` and writes a visible `rumoca-workspace.toml` for the
  staged examples. When the pinned MSL/CMM libraries are present under
  `target/`, it writes a parsed source-root cache plus manifest so browser runs
  honor the effective workspace roots without loading raw library source files
  at runtime.
- **Package discovery** probes `<site>/pkg/<subdir>/` (deployed layout)
  and `<repo>/packages/rumoca/dist/<subdir>/` (local repo-root serve), overridable with
  `window.RUMOCA_LIVE_PKG_BASE`. The WASM download happens lazily on first
  interaction. `cargo xtask docs serve` builds the missing local package
  before serving unless `--skip-wasm-build` is passed.
- **Visualizations**: a `viz-radial` fence annotation adds a built-in
  animated cross-section for 1-D array states; a following
  `js,rumoca-viz` fence becomes an *editable* visualization script that
  receives `{ payload, times, names, data, container, api }` (see the
  turkey and 2-D wave pages in the user guide for worked examples).

Authoring a live example:

````markdown
```modelica,interactive
model M ... end M;
```
````

Keep embedded models validated — simulate them with the native CLI before
committing, and give them an `experiment` annotation so browser runs have
sensible defaults.

Local testing with the WASM parts active: run `cargo xtask docs serve` and
open the printed guide URL. A manual browser smoke test
covering the live widgets lives at
`packages/playground/tests/book_live_smoke.mjs`.

## Updating the Pages Artifact

`cargo xtask docs build` runs inside the WASM CI job for both books after
`cargo xtask repo modelica-deps ensure`; broken book builds therefore block
the Pages deployment rather than shipping. The symlinked `live/` assets are
copied into each book's output with hashed filenames automatically.
