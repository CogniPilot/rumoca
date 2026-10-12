# Installed-wheel and strict CPU exports at a5

Source `a5cb641789fbcee2176fabe26796aade2995e5e5` passed the canonical
installed-wheel suite and all three strict generated-backend tests. All 4,130
tracked files remained byte-identical before and after execution. This is an a5
checkpoint, not qualification of a later scheduling candidate or full CI.

The fresh Linux wheel was built with `maturin build --manifest-path
crates/rumoca-bind-python/Cargo.toml --compatibility linux`, then consumed by
`cargo xtask python test --python <strict-venv-python> --wheel <fresh-wheel>
--require-guide` inside `nix develop .#full`. The actual wheel import reported
build identity `a5cb641789fb`. All eight required scripts passed: smoke, FMI
source codegen, public API, magic, exports, guide, authored scenarios/CMake,
and neural benchmark. This includes ten Python guide fences, four unchanged
scenario TOMLs and two exact CMake teaching contexts. FMI source generation
is not an FMU conformance result.

Each exact selector below used the same strict marker and Cargo command:

```sh
cargo test --locked --offline --jobs 4 -p rumoca \
  --no-default-features --features template-runtime-tests \
  --test suite_template_runtime SELECTOR -- --exact --nocapture
```

| Exact selector | Passed cells | Rust runtime | Guardian elapsed |
|---|---:|---:|---:|
| `workshop_numerical_examples::jax_actual_workshop_examples_jit_initialize_and_differentiate` | 5 | 196.97 s | 465.71 s |
| `workshop_numerical_examples::casadi_actual_workshop_examples_initialize_evaluate_and_differentiate` | 5 | 23.76 s | 25.15 s |
| `backend_template_runtime_regression::jax_ode_target_handles_reported_explicit_algebraic_assignments` | 1 | 1.38 s | 2.64 s |

Exactly three Rust tests passed, with zero failed or ignored tests. The five
workshop sources/defaults were SympyDecay, NeuralODETensor (`nHidden=32`),
NeuralPredatorPrey (`nHidden=24`), NeuralLatentOscillator (`nHidden=128`) and
NeuralODEBackprop (`nHidden=12`, `nBatch=8`). JAX executed initialization, RHS,
JIT, forward JVP and reverse AD; CasADi executed initialization, RHS and symbolic
Jacobians. Original #418 executed JIT and forward/reverse Jacobians. Missing
required tools could not be skipped.

Cold prerequisites were timed separately: fresh wheel build 551.55 s and
same-source release CLI build 535.49 s. The canonical eight-script suite took
332.08 s including its xtask compilation. The first export gate included
267 s of native compilation. These elapsed times are not model runtime.

The unchanged Backprop1k benchmark used width 30, batch 8, horizon 0.2,
interval 0.02, three iterations and one warmup. It validated all six visible
observables at all eleven native CSV samples, plus all 1,082 JAX manual/reverse
gradients. Final observable maximum absolute error was `1.1623731491217093e-14`;
manual/reverse gradient maximum error was `5.204170427930421e-18`. Native
internal-state gradient parity remains unavailable through the visible-output
CSV. Actual model compile was 0.369 s, preparation 5.443 s and hot simulation
average 0.687 s. Cold CLI build is excluded from those figures. The report and
log are retained; the validated CSV was temporary and was not retained.

Actual tools were Python 3.12.13, JAX 0.9.2 on CPU, CasADi 3.7.2, NumPy 2.4.3,
maturin 1.12.6, native CMake 4.1.2 and Cargo 1.95.0-nightly. All phases exited
zero without resource stops. Focused guards remained 600 s; compound build
and suite guards used the explicitly recorded 1,800 s outer envelope. Both
retained 4,096 MiB aggregate RSS plus swap, 8,192 MiB target/scratch limits,
16,384 MiB host reserve and four workers. Per-model watchdogs were unchanged.
Maximum observed group RSS plus swap was 3,462,712 KiB and target size was
5,576,318,976 bytes. Terminal audit found zero visible target references;
418 denied process reads remain unobserved. The cache is retained for refresh.

[The compact receipt](python-exports-a5-checkpoint.json) binds actual wheel,
CLI, test ELF, build identity, source manifests, tools, commands, logs and
resource records by SHA-256. Raw evidence is retained under
`$HOME/scratch/rumoca/workshop427-python-exports-a5-20261012/evidence`.
Browser, hardware GPU, later-tip wheel and final landing gates require their
own receipts.
