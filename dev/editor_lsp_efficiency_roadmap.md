# Editor LSP Efficiency Roadmap

## Goal

Make Modelica library-backed editing responsive in both VS Code and the wasm editor without dropping core language features whenever MSL or other large libraries are present.

## Current problems

- VS Code still performs expensive library-backed semantic work on latency-sensitive request paths.
- `import Modelica.` completion can still force a full-session resolve after the live-diagnostics mitigation.
- The wasm editor still loads libraries through a per-file `add_document` path instead of the batched parsed-library path used by the native compiler.
- Library state and mutable editor state are not cleanly separated across the native and wasm stacks.
- Regression coverage is uneven: Rust tests cover some LSP behavior, but editor-level library loading is only lightly exercised.

## Target architecture

- Keep library state immutable, batched, and separately indexed from open editor buffers.
- Keep parse/lint feedback immediate and cheap.
- Run semantic compile diagnostics asynchronously, on debounce or save, with version-aware cancellation.
- Resolve import/completion requests from a lightweight symbol index instead of full-session recomputation.
- Reuse the same library-loading and session architecture across native LSP and wasm worker code.

## Phase 0: Stabilize the current path

- Keep the live compile-diagnostics guard as a temporary backpressure mechanism, but treat it as a stopgap.
- Expand VS Code path handling: normalize absolute paths, expand `~`, and reload library configuration on settings changes.
- Stop doing library parse/index work directly on `didOpen` and every `didChange` when the library inputs have not changed.
- Add regression tests for large-session live diagnostics and wasm library loading.

## Phase 1: Shared library index

- Introduce a shared library-index builder in `rumoca-session` that produces:
  - parsed definitions
  - package and class symbol lookup tables
  - import and completion lookup tables
- Make both `rumoca-lsp` and `rumoca-bind-wasm` consume that index instead of rebuilding completion state from the full session.
- Replace completion fallback paths that call full-session class enumeration with index lookups.

## Phase 2: Split immutable and mutable session state

- Separate library documents from workspace and open documents so user edits do not invalidate the whole resolved library world.
- Add source-set level invalidation and caching boundaries for:
  - immutable library source sets
  - mutable workspace source sets
  - transient active-document overlays
- Reuse `replace_parsed_source_set` style APIs everywhere libraries are loaded.

## Phase 3: Request scheduling and cancellation

- Move semantic diagnostics off the critical typing path.
- Add debounce and cancellation keyed by document version for:
  - diagnostics
  - completion precompute
  - hover and definition on cache miss
- Keep save-time semantic diagnostics, but make them incremental and bounded to the active top-level model where possible.
- Prefer incremental text sync for transport efficiency after the semantic work is decoupled.

## Phase 4: Wasm parity

- Stop using per-file `add_document` library loading in the wasm editor UI.
- Route wasm library imports through parsed batches and the shared library index.
- Move library parsing onto the existing worker-based pipeline and keep UI-thread work limited to progress and reporting.
- Ensure wasm completion, hover, and diagnostics use the same library index and scheduling rules as VS Code.

## CI and regression gates

- Keep the large-session LSP diagnostics tests in `rumoca-tool-lsp`; they already fit the main workspace CI job.
- Add wasm regression tests that prove:
  - `load_libraries` loads a usable library source set
  - `compile_with_libraries` actually honors supplied libraries
- Run generated wasm package smoke tests in CI, not just Rust unit tests, so JS binding regressions are caught too.
- Add a future bounded performance harness with:
  - a medium synthetic library fixture
  - an import-completion latency check
  - a live-diagnostics no-full-resolve assertion

## Developer commands

- `cargo test -p rumoca-tool-lsp`
- `cargo test -p rumoca-bind-wasm`
- `rum wasm test`
- `rum vscode edit`

## Exit criteria

- Opening a simple `import Modelica.*` file with MSL configured does not peg a core in VS Code.
- Import completion works without full-session resolve on every trigger.
- Wasm library loading uses the same batched and indexed model as native.
- CI fails on regressions in both the LSP large-session path and the wasm library-loading path.
