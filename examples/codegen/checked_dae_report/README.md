# Checked DAE Report Target

This worked custom-target example renders the canonical checked DAE template
projection as a readable report. It consumes typed variable identities,
attributes, expression counts, and every semantic system without simulating or
inventing a second runtime interface.

```bash
cargo run -p rumoca -- \
  compile examples/models/SympyDecay.mo \
  --model SympyDecay \
  --target examples/codegen/checked_dae_report \
  --output examples/codegen/gen/sympy_decay_checked_dae_report
```

Browser-native simulation belongs on the planned FMI 3/Wasm execution path;
this target is deliberately an inspection/code-generation example rather than
an independent JavaScript solver.
