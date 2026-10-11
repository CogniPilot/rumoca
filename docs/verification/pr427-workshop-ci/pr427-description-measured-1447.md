## Branch Naming

`codegen-targets-exports-and-editor-rendering`; report source `0db939b75deb0bf8b018c0378a2324537310f1c7` (measurements executed at their named earlier sources), against `origin/main` at `0b249f0f725431613c25bb56e40fbbe0df54c4d2`.

## Summary

Render complete workshop source projects through Python, LSP and browser APIs, with checked capabilities and source locations. ODE targets support explicit algebraic assignments, initialization and pure-call projection. Modelica base/flat/DAE exports preserve identifiers, arrays, modifiers and derivative elements. The JAX scenario now uses existing event-free SympyDecay; original Ball keeps its honest event refusal. Remove the empty notebook.

Fix shared tensor writes, selected conditional differentiation, iterator declaration identity, wide Integer arithmetic, bounds diagnostics and native `memcmp` registration. Ordinary example CI now compiles the actual 13 tracked files/15 standalone entrypoints with authored defaults, plus independent analytic traces and authored Kalman initialization assertions. This replaces no original model source or unsupported target guard.

## Spec / MLS Alignment

Checked active contracts: SPEC_0001/0002, 0007/0040, 0008, 0018, 0021, 0022, 0025, 0029/0041, 0032, 0033/0050. SPEC_0036/0037 remain drafts. MLS areas: arithmetic/conditionals (§3), delay (§3.7.4), modifiers (§7.2.5), initialization (§8.6), array constructors (§10.4.1), functions/algorithms (§11–12), clocks (§16.5).

Resolve owns declaration IDs; Flatten preserves them; DAE constructs compact domains; Solve registers exact expression owners; eval-solve owns canonical execution and ABI checks; Cranelift emits checked native operations; templates consume those owners.

## Risk and Design Notes

Correctness risks are lost identity, selected activation, tensor aliasing and Integer precision. The 87-file iterator migration separates Source/Generated: Parse AST binder is None, Resolve issues Source(DefId), and Core requires explicit provenance. Missing provenance/Source(0) are rejected; generated producers declare Generated. Parsed-cache schema is 7; DAE/Solve schemas remain 44/74. Old Core layouts are refused.

New APIs include `IndexBinderProvenance`, checked scalar ABI helpers, call-aware rendering/capabilities and browser source/workspace rendering. Existing browser `render_target` arguments remain unchanged. Maintenance risk is profile growth: Integer Mod/Rem retain native refusal, and Interpreter/Auto does not prove native execution. Neural defaults remain unchanged: Tensor nHidden=32; Backprop nHidden=12/nBatch=8 (218 trainable states). Larger named variants have separate compilation coverage.

## Testing

[Measured gate receipts](measured-gates-1447.json) is source-pinned:

- `2a0cbb0fd`: Node3/LSP4 passed; workshop Rust2 passed with 28 OMC cells/168 retained source, CSV and log files (6 originals +18 exports + independent original/3-export derivative fixture). Native snapshot/staging/scalar22 passed; browser7 passed/1 oracle failure.
- `a137a6d6b`: memcmp1 passed; original runtime9 had 4 passes/5 failures.
- `a1ea61d8a`: typed bounds and one-based fallback2 passed. `4fdc7cb44`: membership before1 failed.
- `673ce6ec`: compiler6, native projection/guard4, source bounds1 and shared scalar projection3 passed. `a070366dd`: valid-false-guard before failed, actual13 versus expected3.

At `1447`, update1/membership3/normal-for1/canonical Integer3/native Integer5 passed. Public14 measured **12 passes/2 failures**: all ten original issue regressions passed; extra controls hit ED018 dynamic-divisor and EL005 B.1c profile refusals. C13 Auto and all eleven U4 samples explicitly ran Cranelift. Browser8/catalog1/renderer1 subsequently passed at the same1447 source. The example CLI stopped during compilation at the8GiB scratch floor (exit137); its seven tests and fifteen entrypoints have no measured pass yet. Test-only Integer control corrections and the two-ELF GNU static-PIE lane remain unrun. Key commands are `cargo xtask verify examples`, default workspace tests, `cargo xtask playground test`, and `cargo xtask verify template-runtimes --backend modelica --require-external-tools`. Existing CI selects actual LSP4, Node3, Modelica2 and the new example suite7; C/JAX/CasADi lanes select actual numerical examples. Manual 32-model/246-cell claims are not CI proof.

Final coverage/verify-gate, fmt/clippy/workspace/doc, fixed MSL canary, full MSL/ModelicaTest and pinned corpus gates remain unqualified here. No full421/cohort or merge-readiness claim. Original printed #424 is dimensionally invalid; its conforming reverse-order variant remains a real repaired defect. #425 memory/time and actual static-PIE execution remain unmeasured.

## Code Size Budget

```text
production_lines_added: 8058
production_lines_deleted: 1425
test_lines_added: 7734
test_lines_deleted: 58
documentation_lines_added: 3096
documentation_lines_deleted: 7
public_items_added: 33
public_items_removed: 10
files_touched: 303
net_added_lines: 17398
```

[Metrics/inventory](source-metrics-0db939b75.json) reconcile with rename-aware Git numstat. These source-pinned metrics precede this report-only commit. Public counts are 27 unrestricted declarations/reexports plus six fields; lexical limits are recorded, without a reachable-API/semver claim. Growth supplies target support, authentic regressions and retained evidence. Compression removes parallel inliner, tensor-write and name-search paths in favor of shared owners; preserve controls during further consolidation.

## Reviewer Checklist

- [x] Ownership, migration, source metrics and limits documented.
- [ ] Final-tip focused results, coverage, required CI/MSL gates and approving review complete.
