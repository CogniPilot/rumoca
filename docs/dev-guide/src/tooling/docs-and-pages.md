# Docs and Pages

## Site Layout

GitHub Pages serves one artifact assembled by the WASM build job in CI:

```text
https://cognipilot.github.io/rumoca/            ← playground (editors/wasm)
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
mdbook build docs/user-guide
mdbook build docs/dev-guide
cargo xtask verify docs        # the docs CI gate
```

## Live Examples

Fenced blocks annotated `modelica,interactive` become editable, runnable
mini editors backed by the WASM package. The runner is
`docs/user-guide/live/rumoca-live.js` (+ `.css`), wired into both books via
`additional-js`/`additional-css` in their `book.toml`s —
`docs/dev-guide/live` is a symlink to the user-guide copy so there is a
single source.

How it works:

- **Editors** are Monaco (same CDN as the playground), using the shared
  Modelica language definition
  `editors/wasm/src/modules/modelica_language.js`, with a plain-textarea
  fallback when the CDN is unreachable.
- **Language services** (completion, hover, diagnostics-as-markers) call
  the `lsp_*` functions of `rumoca-bind-wasm` directly on the main thread —
  book examples are small, so no worker is needed.
- **Simulate** calls `simulate_model(source, model, 0, 0, "")`, deferring
  to the model's `experiment` annotation; results render as an inline SVG
  plot. **Show DAE** renders the `dae-modelica` target.
- **Package discovery** probes `<site>/pkg/<subdir>/` (deployed layout)
  and `<repo>/pkg/<subdir>/` (local repo-root serve), overridable with
  `window.RUMOCA_LIVE_PKG_BASE`. The WASM download happens lazily on first
  interaction.
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

Local testing with the WASM parts active: build the package
(`cargo xtask wasm build`), build the books, then serve the **repository
root** (e.g. `python3 -m http.server`) and open
`docs/user-guide/book/index.html` from there. A manual browser smoke test
covering the live widgets lives at
`editors/wasm/tests/book_live_smoke.mjs`.

## Updating the Pages Artifact

`mdbook build` runs inside the WASM CI job for both books; broken book
builds therefore block the Pages deployment rather than shipping. The
symlinked `live/` assets are copied into each book's output with hashed
filenames automatically.
