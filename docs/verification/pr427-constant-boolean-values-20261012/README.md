# Constant scalar Boolean values (#403)

The flat constant-function interpreter evaluated both Boolean operands before phase-DAE could normalize the value. With the original guarded array access and k=4, both And and Or failed at index4 of a three-element array. The corrected BEFORE measured 1 passing fault control and 2 failing inactive-index controls.

The six-line production change shares the existing private determining-value predicate with the function interpreter. It evaluates the left operand once and skips the right operand only for actual scalar Boolean false/And or true/Or. Selected and independently required invalid indices still fail. Arrays retain the existing evaluation path. No public API, dependency, schema, or model-specific branch is added.

Clean source `5b129f969e9dac54a3ae02d4941af1cc5bd07df1` passes focused3/3, full eval-flat185/185, scoped all-target/all-feature Clippy, and pinned workspace rustfmt. The focused tests are included in185. All source manifests are identical before and after each gate; no resource stop occurred.

Earlier receipts remain immutable: the first BEFORE had a missing test expression span; the first AFTER returned the correct integer-literal values but two new assertions expected the Real variant. Separate test-only commits correct these fixture representations without changing the original function, indices, numeric expectations, or fault controls.

This receipt measures the canonical translation-time function-call evaluator. Its parameter-binding caller is source-reviewed; no additional whole-model source-to-DAE parameter execution, native, WASM, exporter, or final combined validation is claimed here. SPEC0022 EXPR-042 governs the chosen policy; SPEC0033 governs the retained BEFORE/AFTER evidence. `receipt.json` binds all raw logs, guardians, recipes and source manifests outside the disposable target using paths relative to `$HOME`.
