# Targets and Templates

Rumoca code generation is target-directory based. A target directory contains a
`target.toml` file and Jinja templates. The target owns the IR stage it expects;
individual template files should not silently switch IRs.

List built-in targets:

```bash
cargo run -p rumoca -- targets
```

Render a built-in target:

```bash
cargo run -p rumoca -- \
  compile examples/models/SympyDecay.mo \
  --model SympyDecay \
  --target sympy \
  --output /tmp/sympy_decay
```

Render a custom target directory:

```bash
cargo run -p rumoca -- \
  compile examples/models/SympyDecay.mo \
  --model SympyDecay \
  --target examples/codegen/standalone_web \
  --output /tmp/sympy_decay_custom
```

Runnable codegen scenarios live under `examples/codegen/` and write generated
files under `examples/codegen/gen/`, which is ignored by git:

```bash
cargo run -p rumoca -- \
  compile examples/models/SympyDecay.mo \
  --model SympyDecay \
  --target examples/codegen/standalone_web \
  --output examples/codegen/gen/sympy_decay_standalone_web
```

## Custom Targets

For custom generation, create a directory with `target.toml` and templates, then
pass the directory as `--target`.

The repository includes `examples/codegen/standalone_web/target.toml` as a
minimal custom target bundle that renders both standalone HTML and companion
JavaScript. `examples/codegen/custom_casadi.jinja` shows the simpler direct raw
template workflow when a `target.toml` bundle is unnecessary.

Rust code should not special-case target languages such as C, MLIR, CUDA, or
Python. Language-specific behavior belongs in `target.toml` metadata and the
templates for that target.
