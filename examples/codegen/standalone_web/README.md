# Standalone Web Target

This custom target renders a standalone HTML page plus companion JavaScript.
It demonstrates a target directory with `target.toml` and local template files.

```bash
cargo run -p rumoca -- \
  compile examples/models/SympyDecay.mo \
  --model SympyDecay \
  --target examples/codegen/standalone_web \
  --output examples/codegen/gen/sympy_decay_standalone_web
```

Output is written to `examples/codegen/gen/sympy_decay_standalone_web/`.
