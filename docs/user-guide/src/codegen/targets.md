# Targets and Templates

Rumoca can render a compiled model into symbolic frameworks, compiled
simulation kernels, eFMI artifacts, or Modelica source. Code generation is
*target-directory based*: a
target is a `target.toml` manifest plus Jinja templates, and each target
declares which compiler IR stage it consumes.

## Listing Targets

```bash
rumoca targets
```

Built-in targets include:

| Target | IR | Mode | Output |
|---|---|---|---|
| `casadi-ode` | solve | symbolic | Differentiable CasADi explicit RHS |
| `jax-ode` | solve | symbolic | JIT/AD-capable JAX explicit RHS |
| `rust-fixed-ode` | solve | compiled | Fixed-size, allocation-free Rust explicit-ODE derivative kernel |
| `rust-ode` / `c-ode` | solve | compiled | Checked explicit-ODE derivative kernels |
| `cuda-ode` | solve | compiled | Batched CUDA explicit-ODE derivative kernel |
| `wgsl-ode` | solve | JIT | Experimental WebGPU explicit-ODE kernels for browser execution |
| `mlir` | solve | source | Inspectible MLIR solve-kernel source with affine tensor loops |
| `flat-modelica` / `base-modelica` | flat | source-transform | Flattened Modelica-family interchange artifacts |
| `dae-modelica` | dae | source-transform | Modelica representation of the checked DAE |
| `fmi2` / `fmi3` | fmi | standards container | Source-code Model Exchange and Co-Simulation FMUs |
| `fmi-ls-wasm` | fmi | compiled component | Experimental FMI-LS WebAssembly component crate |
| `galec` / `galec-production` | algorithm-code | eFMI | Algorithm Code and Production Code eFMU containers |
| `embedded-c-galec` | algorithm-code | compiled | GALEC-derived embedded C without an eFMI container |

Targets without a complete checked artifact and executable or independent
validation evidence are intentionally absent. Rumoca does not expose aliases
for removed target names or route those names through a weaker IR.

The `rumoca targets` table also reports a readiness level (0 = experimental
… 2 = validated) and per-feature support columns (scalarization, tensor
features such as matmul/linear solve/elementwise/stencil kernels, events, AD,
…) for each target. Treat the table — not this page — as the current source of
truth.

## Rendering a Target

```bash
rumoca compile examples/models/SympyDecay.mo \
  --model SympyDecay \
  --target c-ode \
  --output /tmp/decay_c_ode
```

`--output` may be a file or directory depending on what the target renders.

## The rust-fixed-ode Event Runtime

- `Simulator` advances the model by fixed communication steps with events:
  `Simulator::new(method, location, settings, t0, y, p)`, `initialize()` (the initial
  event), then `do_step(h)`, which lands exactly on `t + h` and returns the
  number of events it applied. Inputs and tunable parameters are written
  through `p_mut()` between steps.
- `typed` names the slots: records `Parameters`, `Inputs`, `States`,
  `Outputs` and `Discretes` with one field per model variable of that role
  (dots become underscores, Rust keywords get `r#`, arrays are row-major
  `[f64; N]`, Integers and Booleans are `f64`), documented with its
  description. Constants and algebraic variables are not fields. Each record
  has `read(&slots)` and `write(&self, &mut slots)` over `State` or
  `Parameters`; `Parameters::default()` reads `DEFAULT_P`, the others default
  to zero. Names with no Rust spelling (`self`, quoted identifiers) and two
  names spelling one field are refused.
- `typed::Model` wraps `Simulator`: `Model::new(&parameters)` (RK4,
  `Bracketed`, default settings) or `Model::with(method, location, settings,
  &parameters)` runs `init_params`, `init_state` and the initial event with
  zero inputs at time 0; `step(&inputs, dt, substeps)` holds the inputs over
  `substeps` equal steps and refreshes algebraics; `states()`, `outputs()`,
  `discretes()`, `time()`, `simulator()` and `simulator_mut()` read it.
  `init_state` writes only starts computed from parameters: other states
  start at 0, so a literal `start` is not applied.
- `Method` is `Euler`, `Heun`, or `Rk4`. A step is clipped to land on every
  time event (static instants and `sample` clock ticks).
- `EventLocation` decides where a state event found at a step end is applied:
  `StepEnd` (no extra evaluations, up to one step late), `Secant` (at most
  `settings.secant_trials` refinements on the step's cubic Hermite interpolant), or
  `Bracketed` (refined to the roundoff of the step). Located events land past
  the crossing, then the rest of the step is integrated.
- The model's facts (roots, relation memories, pre lanes, discrete rows, the
  issued event iteration schedule) are generated; the event algorithm is
  the hand-written `events.rs`, copied in unchanged. It applies the same
  event rules as the FMI C target, and finds where a state event crosses
  zero with a copy of `rumoca-ir-solve`'s `RootBracket` search, including its
  `evaluation_bound`.
- `derivative_rhs_into` and `Simulator::do_step` perform no heap allocation.

Events cover everything the compiler's scalar event profile admits, as the
FMI C component executes it: relation memory and its mode search, pre lanes,
discrete and guarded rows, pulses, observation rows, clocks and static time
events, discrete-valued inputs, error- and warning-level assertions, and the
location plan's sliding modes. Discrete rows whose `pre` is held for a pass
(`Fixed`, as stick-slip state machines lower) run; rows that read `pre` at
the event entry in every pass (`EventEntry`) are refused. The profile itself
refuses delays, `terminate`, dynamic time events, structured updates, event
transactions and reads of `initial()` outside a checked relation region.
Assertions raised inside called functions, and user-function calls the
compiler does not inline into event rows (roots, `when` bodies, `reinit`,
assertions), are refused here.

A `reinit` is copied into its state at the end of each event pass, so it
reads the state's left limit (MLS 8.3.6). After a static time event the model
reads `time` at the instant's right limit. Static instants and clock ticks are
fixed when the module is generated, from the parameters' values then; writing
a parameter that sets one does not move it.

The target does not implement linear solves, initialization residuals,
residual equations, or callbacks. More than `settings.max_events_per_step` state events in
one step (Zeno behaviour) end in `SimError::TooManyEvents`. A crossing that leaves
and returns within one step is not seen. Unsupported input is rejected before
source is written.

The module needs Rust 1.86 or later. The event iteration keeps about twenty
copies of `State` and `Parameters` on the stack, about 160 bytes per slot of
`Y_LEN + P_LEN`; size the calling thread's stack for large models.

### Generating rust-fixed-ode from `build.rs`

A crate can render the module at build time with a prebuilt `rumoca`. Cargo
rescans a `rerun-if-changed` directory recursively, so naming the model
directory, every source root and the executable rebuilds exactly when one of
them changes. The module starts with inner attributes, which `include!`
rejects, so `build.rs` also writes a `#[path]` declaration for it:

```rust
// build.rs
use std::{env, fs, path::PathBuf, process::Command};

fn main() {
    let rumoca = PathBuf::from(env::var_os("RUMOCA").expect("set RUMOCA"));
    let out = PathBuf::from(env::var_os("OUT_DIR").unwrap());
    println!("cargo:rerun-if-env-changed=RUMOCA");
    println!("cargo:rerun-if-env-changed=MODELICAPATH");
    println!("cargo:rerun-if-changed={}", rumoca.display());
    println!("cargo:rerun-if-changed=models");
    // Also one rerun-if-changed per --source-root or MODELICAPATH entry.
    let status = Command::new(&rumoca)
        .args(["compile", "models/Plant.mo", "--model", "Plant"])
        .args(["--target", "rust-fixed-ode", "--output"])
        .arg(&out)
        .status()
        .unwrap();
    assert!(status.success());
    let module = out.join("Plant_fixed_ode.rs");
    fs::write(
        out.join("plant.rs"),
        format!("#[allow(clippy::all, unused_parens)]\n#[path = {module:?}]\npub mod plant;\n"),
    )
    .unwrap();
}
```

```rust
// lib.rs
include!(concat!(env!("OUT_DIR"), "/plant.rs"));
pub use plant::typed::{Inputs, Model, Parameters, States};
```

## Codegen Scenarios

Like simulations, generation jobs worth repeating belong in a `rumoca-scenario.toml`
with `task = "codegen"`. Runnable examples live under `examples/codegen/`
and write into `examples/codegen/gen/` (git-ignored):

- `examples/codegen/rumoca-scenario.sympy_decay_jax_ode.toml` — checked ODE RHS JAX target
- `examples/codegen/rumoca-scenario.sympy_decay_c_ode.toml` — checked ODE RHS C target
- `examples/codegen/rumoca-scenario.sympy_decay_checked_dae_report.toml` —
  custom checked-DAE report target
- `examples/codegen/rumoca-scenario.sympy_decay_custom_checked_variables.toml`
  — raw checked-DAE Jinja template

## IR Dumps vs Targets

If what you want is to *see* a compiler stage rather than generate project
code, use `--emit` instead of a target — see
[Inspecting and Debugging Models](../simulation/inspect.md).

### Callable WGSL for algebraic models

The WASM compiler with the `native-assignments` feature exports
`prepare_gpu_value_function(source, model_name)`. For supported stateless
Real models it returns JSON containing the WGSL source for
`rumoca_evaluate(p, time)`, the variable layout, parameter defaults and
compiler provenance. The caller supplies its own compute entry point, bindings
and dispatch. Use the P and Y **element indices** from the variable layout to
pack inputs and read results. WGSL values are f32, including time. The native
solver's byte offsets describe f64 storage.

Preparation requires a compiler-issued direct-assignment schedule with dense
targets. Unsupported models produce an error at preparation.
