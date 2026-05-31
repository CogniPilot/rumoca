# Docs and Pages

GitHub Pages is built from the existing WASM editor artifact. The playground
stays at the Pages root:

```text
https://cognipilot.github.io/rumoca/
```

The mdBook outputs are deployed under subdirectories in the same Pages artifact:

```text
https://cognipilot.github.io/rumoca/user-guide/
https://cognipilot.github.io/rumoca/dev-guide/
```

CI builds the books with `mdbook build` during the documentation gate and again
when preparing the Pages artifact. This keeps broken book links from reaching a
release deployment.

Local checks:

```bash
mdbook build docs/user-guide
mdbook build docs/dev-guide
cargo xtask verify docs
```

Pages deployment runs from the main CI release path. The deploy job publishes
the artifact uploaded by the WASM build job, so any new public static content
should be copied into `gh-pages/` in that job.
