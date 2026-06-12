# Code Generation Engine

`rumoca-phase-codegen` renders text from IRs through Jinja templates. A
*target* is a directory: a `target.toml` manifest plus templates. Built-in
targets are bundled into the binary; users can supply their own directory
or a raw template (see the user guide's
[Custom Targets](https://cognipilot.github.io/rumoca/user-guide/codegen/custom-targets.html)
chapter for the user-facing workflow).

## Ownership Rules

- **The target owns its IR choice.** A manifest declares which stage it
  consumes (`ast`/`flat`/`dae`/`solve`); individual templates may not
  silently switch IRs. Targets consume the lowest IR they need — no lower.
- **The engine renders; it does not decide semantics.** Scalarization,
  structural analysis, and lowering happen in compiler phases; templates
  receive prepared data.
- **No language special-casing in Rust.** Phase code must not branch on
  "is this C/CUDA/Python". Language-specific behavior is expressed in
  `target.toml` metadata and the templates themselves
  ([SPEC_0029](https://github.com/CogniPilot/rumoca/blob/main/spec/SPEC_0029_CRATE_BOUNDARIES.md)).

## Capability Declarations

`rumoca targets` prints, for every built-in target, the IR it consumes,
its generation mode (symbolic / compiled / source-transform / packaged),
deployment class, readiness level (0 experimental … 2 validated), and
per-feature support columns (scalar, matmul, linsolve, sparse, dynamic
control flow, events, forward/reverse AD). These come from the target
manifests — keep them honest when extending a target; the table is the
user-facing contract.

## Template Runtime Tests

Targets whose output is executable are covered by opt-in template-runtime
regression tests (`cargo xtask verify template-runtimes`), which actually
run the generated code. Adding a target with runnable output should come
with such a test.

## Adding a Target

1. Start from a worked example: `examples/codegen/standalone_web/` is a
   complete custom bundle; the built-in target directories show the full
   manifest vocabulary.
2. Declare the IR stage and capabilities in `target.toml`.
3. Write templates against the serialized IR (`--emit <stage>-json` shows
   the exact shape).
4. Wire a runtime regression test if the output executes.
