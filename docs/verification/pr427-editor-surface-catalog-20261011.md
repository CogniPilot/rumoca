# PR 427 editor surface catalog correction

Actual CI run [38114997762](https://github.com/CogniPilot/rumoca/actions/runs/38114997762),
head `32f75cfa310ac2f1a4b12c874093038203de1b0c`, editor job `114398094596`,
failed at 06:19:35 UTC with `WASM surface validation drifted: render_target
[bind-wasm:render_target]`. Downloaded raw job log:
`$HOME/scratch/rumoca/pr427-cold-editor-triage-38114997762-20261011/editor.log`,
SHA-256 `ab27645fac3476505046d0b2800961e2c7b67d7fc0c9fb0d3057b4c5c97014e6`.

The catalog searched `lib.rs` for the definition `pub fn render_target(`.
The implementation moved to `target_render_api.rs`; `lib.rs` retains its public
re-export. The exact definition pattern exists in the new owner, and the exact
existing proof pattern remains in `tests.rs`. The repair changes only the
catalog's source path, preserving the API, signature, proof and validator.

This is a stale catalog location, not the separate PR 421 cold Resistor CodeLens
timeout. `run_lsp_msl_completion_timings` calls the VS Code and WASM runtime
smokes before checking the surface catalog; those calls returned successfully
before this validator error. This receipt does not substitute that control-flow
fact for the missing final benchmark report or claim any runtime timing count.

The focused test calls the existing `build_surface_coverage_report` against
both actual source/proof catalogs. It requires nonempty reports with every
declared entry. It catches relocation drift without building editor runtimes.

```bash
cargo test --locked --offline --jobs 4 -p xtask \
  lsp_benchmark_cmd::tests::editor_surface_catalogs_resolve_actual_implementations_and_proofs \
  -- --exact --nocapture
```

Source-only validation: rustfmt and `git diff --check`. The Rust validator test
has not run yet. No source outside the separate catalog worktree, including the
frozen workshop source, was modified. SPEC_0029 ownership, SPEC_0021 complexity,
SPEC_0033 evidence and SPEC_0025 review requirements were checked.
