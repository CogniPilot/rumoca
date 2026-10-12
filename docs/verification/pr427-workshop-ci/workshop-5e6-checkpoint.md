# Workshop browser, wheel and exports checkpoint at 5e6

Source `5e6f9cb9afec4caf992a370db78c1a5b342d5336` passed the optimized
FullWeb build, three artifact smokes, all four mandatory guide browser scripts
(all 16 authored cells), two native CPU preparation/simulation tests, the eight
required installed-wheel scripts, and all three strict export tests (11 model
cells). All 4,166 tracked files and 6,130 external inputs remained identical
before and after execution. This is focused qualification of this source;
canonical quick failed separately, and full, coverage, later-tip and landing
qualification require their own results.

The actual main WASM, isolated wheel import and release CLI reported build
identity `5e6f9cb9afec`. All 24 optimized main/Diffsol/GALEC package files
(55,550,491 bytes) are sealed outside the build cache. The browser checked
actual downloaded main/runtime/addon hashes against that package. Addons have
no public identity API: their evidence is the actual canonical build plus
artifact hashes. Native test ELFs are copied and hashed; the strict-export ELF
contains no commit string, so its binding is the frozen source/build log and
SHA256, not an invented runtime identity query.

```sh
node packages/rumoca/build.mjs --profile release --variant full-web --optimize
node packages/playground/tests/simulate_smoke.mjs --pkg-subdir release-full-web
node packages/playground/tests/source_root_smoke.mjs --pkg-subdir release-full-web
node packages/playground/tests/target_render_smoke.mjs --pkg-subdir release-full-web
node packages/playground/tests/user_guide_browser.mjs --browser-binary "$CHROMIUM"
cargo test --locked --offline --jobs 4 -p rumoca-bind-wasm --features full-web \
  --lib tests::user_guide_examples_tests::user_guide_gpu_examples_ -- --nocapture
```

The 16 browser cells comprise 14 simulations and two GALEC exports. The
unchanged Booster exercised the real wind knob, R reset and replacement-session
steps, then Space launch with restored defaults. Its integer adjacent phase
sequence was exactly `[0,1,2,3,4]`; 551 nominal intervals produced 552 samples,
safe touchdown at 27.7, and no crash. No exact 18-second touchdown is claimed.
FixedWing keyboard/control routing and HotAirBalloon's real checked-DAE viewer
also passed. CPU and software WebGL remained mandatory. Only the two hardware
WebGPU compute stages skipped after software/fallback adapter admission.
The 20 native guide tests, including slider and wide-model checks, remains owned by
canonical verify full; these two CPU and 16 browser results do not discharge it.

The timing observer separates parsed-cache staging from staged-cache session
compilation/preparation/initialization. FixedWing took 11.063 seconds staging
plus 3.892 seconds session construction; Booster session construction took
2.393 seconds and its full mission cell 21.841 seconds. Modules/caches may be
warm across cells; these are not cold standalone compiler-speed results. The
optimized build took 424.237 seconds and browser wrapper 377.957 seconds.

The fresh wheel used the canonical Python manifest and was installed by
`cargo xtask python test --python <strict-python> --wheel <fresh-wheel>
--require-guide` into a process-owned pip target. The provisioned strict venv
was unchanged. All eight scripts passed, including ten Python fences, four
unchanged authored scenario TOMLs and two CMake contexts. FMI source generation
is not FMU conformance. Separate cold wheel/CLI prerequisites took 432.056 and
318.444 seconds; the eight-script suite took 324.647 seconds. Those builds are
outside the unchanged benchmark child clock.

Each strict selector used:

```sh
cargo test --locked --offline --jobs 4 -p rumoca --no-default-features \
  --features template-runtime-tests --test suite_template_runtime \
  SELECTOR -- --exact --nocapture
```

| Exact selector | Model cells | Rust runtime | Guarded elapsed |
|---|---:|---:|---:|
| `workshop_numerical_examples::jax_actual_workshop_examples_jit_initialize_and_differentiate` | 5 | 232.27 s | 512.977 s |
| `workshop_numerical_examples::casadi_actual_workshop_examples_initialize_evaluate_and_differentiate` | 5 | 24.56 s | 25.797 s |
| `backend_template_runtime_regression::jax_ode_target_handles_reported_explicit_algebraic_assignments` | 1 | 1.63 s | 2.718 s |

JAX checked initialization, RHS, JIT and forward/reverse AD; CasADi checked
initialization, RHS and Jacobians. The unchanged five-model roster/defaults
was SympyDecay, NeuralODETensor (width 32), NeuralPredatorPrey (width 24),
NeuralLatentOscillator (width 128), and NeuralODEBackprop (width 12, batch 8). Exactly
three Rust tests passed, zero failed/ignored. The first export gate includes
279 seconds of feature-graph compilation.

The separate authored Backprop1k benchmark retained width 30, batch 8, horizon 0.2,
dt 0.02, three iterations and one warmup. All 11×6 observable values and 1,082
manual/reverse JAX gradients passed. Maximum final observable error was
`1.1623731491217093e-14`; gradient error was `5.204170427930421e-18`. Actual model
compile/preparation/hot-average times were 0.342/4.414/0.695 seconds. Native
internal-state gradient parity is unavailable through visible CSV. The complete
report is extracted from actual stdout; its temporary CSV was not retained.

Actual tools included Chromium 154.0.8037.92, Python 3.12.13, JAX/JAXlib 0.9.2,
CasADi 3.7.2, NumPy 2.4.3, native CMake 4.1.2 and maturin 1.12.6. All listed phases
exited zero without resource stops. Guards retained four workers, 4 GiB aggregate
RSS plus swap, 8 GiB target cap/scratch floor and 16 GiB host reserve; focused
phases used 600 seconds, the compound wheel suite 1,800 seconds. Per-model
watchdogs, sources and tolerances were unchanged. Terminal native audit found
zero visible references/leases; 417 denied process reads remain unobserved.

[The compact receipt](workshop-5e6-checkpoint.json) binds all package files,
wheel/CLI/ELFs, source seals, recipes, tool versions and raw log/resource hashes.
Raw evidence is retained under
`$HOME/scratch/rumoca/workshop427-final-rebind-5e6f-20261012/evidence`.
No hardware-GPU, full-cohort parity, final-CI or later-source claim is made.
