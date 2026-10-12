# Strict CPU export qualification at 3653

Source `3653c44b2b9864bf993fa9a9bbf2ca578e1df05d` passed all three
strict export tests: three Rust tests, zero failures or ignored tests, and
11 model cells. The two workshop tests each execute the unchanged tracked
sources/defaults for SympyDecay, NeuralODETensor (`nHidden=32`),
NeuralPredatorPrey (`nHidden=24`), NeuralLatentOscillator (`nHidden=128`), and
NeuralODEBackprop (`nHidden=12`, `nBatch=8`). The eleventh cell is original #418.

Each selector below was run with this exact Cargo prefix and suffix, inside
`nix develop .#full` with the provisioned Python environment first on `PATH`:

```sh
cargo test --locked --offline --jobs 4 -p rumoca \
  --no-default-features --features template-runtime-tests \
  --test suite_template_runtime SELECTOR -- --exact --nocapture
```

| Exact selector | Actual result | Rust runtime | Guardian elapsed |
|---|---|---:|---:|
| `workshop_numerical_examples::jax_actual_workshop_examples_jit_initialize_and_differentiate` | 1 test / 5 model cells passed | 200.84 s | 559.18 s |
| `workshop_numerical_examples::casadi_actual_workshop_examples_initialize_evaluate_and_differentiate` | 1 test / 5 model cells passed | 23.75 s | 24.98 s |
| `backend_template_runtime_regression::jax_ode_target_handles_reported_explicit_algebraic_assignments` | 1 test / original #418 passed | 1.34 s | 2.40 s |

The first gate included a fresh native compilation: Cargo reported 5 min 57 s.
The later gates reused that identical test executable. Guardian elapsed includes
compilation, Nix startup and test execution; it is not model runtime.

JAX checks canonical initialization, two RHS points, JIT, forward JVP and a
reverse gradient against native values and finite differences. CasADi checks
initialization, RHS and its symbolic Jacobian against the same controls. The
original #418 checks generated JIT RHS and both forward/reverse Jacobians.
Required dependencies were imported before execution, and the existing strict
template-runtime marker was present; missing dependencies could not count as
a pass. Actual tools were Python 3.12.13, JAX/jaxlib 0.9.2 on CPU, CasADi 3.7.2,
NumPy 2.4.3, rustc 1.95.0-nightly (2026-02-26) and Cargo 1.95.0-nightly.

Each gate retained the existing 600 s timeout, 4096 MiB process-group RSS limit,
16384 MiB host available-memory floor, 8192 MiB target-cache cap and 8192 MiB
scratch-space floor. Cargo, test and Rayon workers were all four; incremental
compilation and dev/test debug information were disabled. All guardians exited
zero without a resource stop. Maximum observed group RSS was 3,462,440 KiB;
maximum cache size was 1,498,001,408 bytes. Terminal inspection found zero owned
target references and zero observed native tasks; 2,268 denied process reads
remain explicitly unobserved.

[The compact receipt](strict-exports-3653-qualification.json) binds the exact
source manifest, tools, commands, logs, guardian/count receipts and copied ELF.
Raw evidence lives under
`$HOME/scratch/rumoca/workshop427-final-exports-20261012/evidence`.
All 4,031 tracked files and nine frozen external bindings were verified after
execution. Generated modules/native snapshots were test-owned temporary files
and were not retained. This record proves the named CPU export tests at 3653;
later commits, installed wheels, browsers, hardware GPU execution and full CI
require their own qualification.
