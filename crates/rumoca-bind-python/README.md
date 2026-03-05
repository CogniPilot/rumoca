# rumoca-bind-python

PyO3 bindings that expose Rumoca compile/lint/codegen capabilities to Python.

## Role in Rumoca
Thin language binding layer for Python users. It adapts core Rumoca crate APIs to Python-callable functions and Python-friendly result/error types.

## Public Surface
- Python module name: `rumoca`.
- Public Python functions: `version`, `parse`, `lint`, `check`, `compile`, `generate_code`.
- Public Python result objects: `ParseResult`, `LintMessage`.

## Inputs
- Modelica source text and model names from Python callers.
- DAE JSON and target template IDs for code generation.

## Outputs
- Python-returned diagnostics and compile/check/lint results.
- Generated target code as Python strings.

## Design Constraints
- Keep logic as wrappers over Rumoca session/phase/tool crates.
- Do not duplicate compiler semantics in binding code.
- Convert Rust errors into predictable Python exceptions/messages.
