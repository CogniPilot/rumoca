# Custom Targets

When the built-in targets do not fit, create a directory containing a `target.toml`
manifest and the templates it references, then pass the directory:

```bash
rumoca compile Model.mo --target path/to/my_target -o out/
```

Every file declaration names an artifact kind, one of the exact IR-crate
contexts (`ast`, `flat`, `dae`, `galec`, or `solve`), and an optional checked
view within that context. The manifest constructor validates those facts before
any template can render. A standalone `.jinja` file is therefore not a target:
it cannot retain the proof that its bytes came from the declared checked view.

An external target consuming Flat is an unregistered, user-authored rendering
extension, not a Rumoca-checked Flat/Base Modelica product. Its files still
declare `semantic_context = "flat"` and the exact closed artifact kind; giving
the directory a retired built-in target name does not restore that product or
its guarantees.

Raw Flat rendering through an external target directory or a `.jinja` template
is an unchecked IR dump produced by the user's template, not a checked Modelica
reconstruction. It is subject to the same equation-body loss that the named
`unsupported-feature:flat-modelica-text-export` refusal cites.

The repository ships a complete worked example:
`examples/codegen/checked_dae_report/target.toml` renders a readable report
from the canonical checked DAE projection.

## Design Rule: Language Knowledge Lives in Targets

Rumoca's compiler phases are deliberately target-agnostic: no Rust code
special-cases C, CUDA, Python, or MLIR. Everything language-specific belongs
in `target.toml` metadata and templates. If a custom target needs
information the IR does not expose, that is a compiler feature request — not
something to hack around in a template.
