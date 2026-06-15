# Troubleshooting and FAQ

## Compilation

**"class not found" / unresolved names** — The model references a package
Rumoca cannot see. Add the library with `--source-root` (CLI), top-level
`source_roots` (scenario), or `rumoca.sourceRootPaths` (VS Code). Check
`MODELICAPATH` if you rely on it. See
[Using Modelica Libraries](./language/libraries.md).

**"Duplicate class 'X' ... with non-identical definition" (EM001)** — Two
files in the same source root define the same class name. This commonly
happens when an old copy of a model sits next to a new one in the same
directory; direct `rumoca sim file.mo` runs include the file's directory as
a source root.

**"structurally singular system: N matched out of M equations"** — The
equation system cannot be matched one-to-one with its unknowns. The
diagnostic names the unmatched equations and unknowns; run
`rumoca compile --inspect structure` for the full matching. Common causes:

- Genuinely unbalanced models (forgotten equation, extra variable).
- High-index DAE formulations that current index reduction does not yet
  handle, such as a Cartesian pendulum with an explicit constraint — see
  [Language Support Status](./language/support-status.md). Reformulating
  with generalized coordinates usually fixes it.

**Model is balanced but produces wrong dynamics** — Dump the system the
solver actually integrates with `rumoca compile --emit dae-mo` and compare
it to your intent. Please file an issue with a minimal model if the lowering
looks wrong.

## Simulation

**"Step size is too small at time = ..."** — The implicit solver stalled,
most often near a dense cascade of state events (rapid relay switching).
Use the explicit solver: `--solver rk-like` or
`annotation(experiment(Solver = "rk-like"))`. For genuinely stiff smooth
systems, try `esdirk34` or `trbdf2` instead.

**NaN/Inf failures** — `rumoca sim` automatically re-runs with NaN tracing
and names the offending variables. To investigate further, evaluate the
model at a chosen point: `rumoca sim Model.mo --inspect eval --at
"x=...@t"`. Typical causes: division by a variable that crosses zero,
`sqrt`/`log` of a negative value, or missing initial values.

**Simulation stops early with a message** — The model called
`terminate(...)`; the message is recorded in the report. `assert` failures
likewise carry their message and source location.

**My run used t_end = 1.0 even though the model has
`experiment(StopTime=...)`** — Native *direct* CLI runs take the end time
from `--t-end` (default 1.0); scenario runs use `[sim] t_end`. The browser
examples and playground do honor the annotation.

## Results

**The HTML report did not open** — It is written next to where you ran the
command (`<MODEL>_results.html` by default, `-o` to override). Open it in
any browser.

**Too many variables in the report** — Add `[[plot.views]]` sections to the
scenario to define focused plots.

## VS Code

**No diagnostics / completion** — Check that the extension is active for
the file (it activates on `.mo` and `rum.toml`). If you enabled
`rumoca.useSystemServer`, ensure `rumoca-lsp` is on `PATH`; otherwise the
bundled server is used.

**Library completion missing** — Set `rumoca.sourceRootPaths` (workspace
scope). For the repository's own examples, run
`cargo xtask repo modelica-deps ensure` first.

## Live Examples in This Book

**The ▶ Simulate button reports the WASM package is missing** — Live
examples need the WASM package deployed next to the book. They work on
[the published site](https://cognipilot.github.io/rumoca/user-guide/); for
a local build, serve the repository root after `cargo xtask wasm build`.

**The editor has no syntax highlighting** — Monaco loads from a CDN; when
offline, the examples fall back to plain text editors but still simulate.

## Performance

**Compilation feels slow on repeat runs** — Check the cache:
`rumoca cache status`. Direct file runs are cached by content; use
`rumoca sim bench` to separate compile time from simulation throughput.

**Browser runs are slower than native** — Expected, especially for large
package trees; use the native CLI for MSL-heavy work.

## Reporting Bugs

File issues at <https://github.com/CogniPilot/rumoca/issues> with a minimal
model. The [Web Playground](./tools/playground.md) is a convenient way to
confirm a reproduction without local setup.
