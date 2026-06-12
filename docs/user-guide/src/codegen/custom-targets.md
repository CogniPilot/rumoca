# Custom Targets

When the built-in targets do not fit, write your own. There are two levels:

## Raw Jinja Templates

For one-off generation, pass a `.jinja` file directly. `--phase` chooses
which IR the template receives (default `dae`):

```bash
rumoca compile Model.mo --target my_template.jinja --phase flat -o out.txt
```

The template gets the serialized IR as its context. The repository example
`examples/codegen/custom_casadi.jinja` shows this workflow.

To learn the available fields, dump the matching IR as JSON first:

```bash
rumoca compile Model.mo --emit flat-json | head -50
```

## Target Directories (`target.toml`)

For anything reusable, create a directory containing a `target.toml`
manifest and the templates it references, then pass the directory:

```bash
rumoca compile Model.mo --target path/to/my_target -o out/
```

The manifest declares which IR stage the target consumes and which
templates render which output files. The target — not individual templates —
owns the IR choice, so a bundle stays consistent.

The repository ships a complete worked example:
`examples/codegen/standalone_web/target.toml` renders a standalone HTML page
plus companion JavaScript from one model.

## Design Rule: Language Knowledge Lives in Targets

Rumoca's compiler phases are deliberately target-agnostic: no Rust code
special-cases C, CUDA, Python, or MLIR. Everything language-specific belongs
in `target.toml` metadata and templates. If a custom target needs
information the IR does not expose, that is a compiler feature request — not
something to hack around in a template.
