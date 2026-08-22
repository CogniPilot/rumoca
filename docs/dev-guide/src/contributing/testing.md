# Testing and Quality Gates

## The Verification Surface

`cargo xtask verify` is the umbrella for everything CI runs:

| Command | Scope |
|---|---|
| `cargo xtask verify quick` | Full CI surface *except* the slow full-MSL parity gate |
| `cargo xtask verify full` | Everything, including full-MSL parity |
| `cargo xtask verify lint` | Formatting + clippy |
| `cargo xtask verify workspace` | Workspace build/tests |
| `cargo xtask verify docs` | Documentation build (rustdoc + mdBook books) |
| `cargo xtask verify msl-parity` | MSL parity gate on its own |
| `cargo xtask verify corpus-pin` | Pinned real-model corpus: the RDD2 flight stack and the MSL canary roster |
| `cargo xtask verify embedded` | Size and precision budget for the embedded flight artifacts |
| `cargo xtask verify template-runtimes` | Opt-in execution tests for generated target code |

Editor surfaces have their own gates:

```bash
cargo xtask vscode test      # extension compile + tests
cargo xtask playground test  # wasm build + browser smoke tests
```

`verify quick`/`full` include the coverage, VS Code, and wasm gates, so
they need the same prerequisites CI installs: `cargo-llvm-cov`, Node/npm,
and the wasm Rust target/tooling.

### The pinned corpus

`verify corpus-pin` compiles the models in
`infra/verification/corpus-pin.json` and compares each one against the
behavior recorded there: the RDD2/Cubs2 flight stack out of the out-of-tree
`modelica_models` checkout, and the twenty-model MSL canary roster in
`infra/verification/msl-canary-20.json`. Every deviation is red in both
directions: a row pinned to compile that stops compiling, and equally a row
pinned to be refused that starts compiling, because the manifest is the
reviewed record of what the compiler accepts.

The flight corpus is out of tree, so its path never appears in this
repository. Give it at run time, either way:

```bash
cargo xtask verify corpus-pin --models-root /path/to/modelica_models
# or, once per checkout:
mkdir -p target/verification
printf '{"models_root": "/path/to/modelica_models"}\n' > target/verification/corpus-config.json
```

A missing corpus is a hard failure with the headline
`corpus unmeasured: the pinned corpus is not on this machine`, never a skip:
a green run that compared nothing would report the corpus as correct.

#### What each row is judged on

A **simulate** row is judged on its pinned readings, and on whether those
readings can see the run at all. A row whose every pinned variable holds one
value over `[0, t_end]` is red as unobserved rather than green: a probe reads
the last sample at or before it, so a run truncated to its first sample would
reproduce every such pin exactly, and so would a compiler that stopped after
initialization. "Holds one value" is measured against the row's own
tolerances, so a pin whose tolerance swallows the whole travel does not count
as seeing it. When a model does nothing inside the corpus default window, the
row states a longer `t_end` and its `why` says why; the three clocked-signal
canaries run to 0.5 for that reason.

A **compile** row is judged on the artifacts it declares under
`expect.artifacts`, not on the exit status alone. Each one names a path
relative to the row's output directory, a `min_bytes` floor, and a kind:
`file` is checked for existence and size, `efmu-container` additionally has to
open as a zip carrying a non-empty `__content.xml` at its root. A compile row
that declares nothing is red for the same reason a simulate row that pins
nothing is.

Every row runs under a deadline derived from `runtime_budget_seconds`: a row's
even share of it with a wide multiplier, never below thirty seconds. A row
that outruns its deadline is killed and reported with the command that hung,
because this gate runs inside `verify quick` and a model that stops
terminating must cost one row rather than the developer loop. The deadline is
a hang catcher, not a performance assertion, so it sits far above the slowest
row the corpus has.

To move a pin, run with `--record`, diff the proposal it writes under
`target/verification/` against the manifest, and copy across only what you
have adjudicated. The gate never edits its own expectations, and it will not
propose a pin it would then call unobserved: a run that moves nothing yields
an empty proposal and a note telling you to choose other observables or raise
`t_end`.

### The embedded budget

`verify corpus-pin` proves the flight models still compile. `verify embedded`
proves the C that comes out still fits on the microcontroller it flies on.
Those are different failures: a projection change can keep every corpus row
green while doubling the scratch struct, or while letting a `double` back
into an inner loop.

Per row of `infra/verification/embedded-budget.json` the gate compiles the
model with the workspace compiler, cross-compiles every emitted `.c` at `-Os`
for Cortex-M7 hard float, and then enforces four things:

- **No warnings.** The generated C advertises warning-free compilation, so any
  output from the cross compiler fails the row.
- **A text ceiling**, on the summed `.text` of every emitted translation unit.
- **A state ceiling**, on `sizeof(<Model>State)` for the target ABI, measured
  by a probe translation unit rather than computed.
- **No forbidden undefined symbol**: no allocator, no `__aeabi_d*` soft-float
  helper, no double-precision libm entry point where the `f` form was required.

Both roots are named on argv and neither has a fallback:

```bash
cargo xtask verify embedded \
  --models-root /path/to/modelica_models \
  --arm-toolchain /path/to/gcc-arm-embedded
```

The toolchain root is the directory whose `bin/` holds `arm-none-eabi-gcc`,
`-size`, and `-nm`. It is required rather than searched for on `PATH` because a
size ceiling is a statement about one compiler: a run that quietly used a
different `arm-none-eabi-gcc` would compare its bytes against a ceiling
measured elsewhere and call the difference a regression.

A row that cannot be measured is a failure naming the row and the command,
never a skip, with the headline `embedded budget unmeasured: …` for a missing
toolchain or models root. `--only <id>` gates a single row.

Ceilings are fall-only in spirit. When size work lands, lower the ceiling in
the same change that lands the saving, so the saving cannot be quietly spent
again. Raising one is not forbidden but is never routine: it needs a reviewed
justification recorded in that row's `budget.measured.comment`, which is also
where the toolchain each ceiling was measured with is named.

## During Development

Plain Cargo works for tight loops:

```bash
cargo test -p rumoca-phase-dae
cargo test -p rumoca-phase-structural some_test_name
```

When testing failure paths, assert the *specific* phase error you expect —
the codebase's expect-vs-error discipline exists so a passing test means
the right thing failed for the right reason.

## The MSL Quality Gate

The strongest regression net is the Modelica Standard Library gate: CI
compiles and simulates a large MSL model population and compares against
recorded baselines, blocking silent regressions in compile success,
simulation success, and trace parity. Details, baseline policy, and
promotion workflow: [MSL Quality Gate](../tooling/msl-quality-gate.md).

For compiler changes that could affect MSL behavior, run the parity gate
(or at minimum `verify quick` plus a targeted MSL model) before opening the
PR — [SPEC_0025](https://github.com/CogniPilot/rumoca/blob/main/spec/SPEC_0025_PR_REVIEW_PROCESS.md)
defines what evidence a PR needs.

## Coverage

```bash
cargo xtask coverage report
```

CI enforces a coverage gate; locally you need `cargo-llvm-cov`.

## Architecture Tests

Dependency boundaries from
[SPEC_0029](https://github.com/CogniPilot/rumoca/blob/main/spec/SPEC_0029_CRATE_BOUNDARIES.md)
are enforced by tests. If one fails on your change, the answer is a design
conversation (possibly a spec change) — not loosening the test.
