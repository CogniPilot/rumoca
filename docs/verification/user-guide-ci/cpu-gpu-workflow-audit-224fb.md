# Guide CPU and optional GPU CI routes

Source-only audit at224fbf99490e0154aae3aca74034bb35540cf48a found no missing
mandatory PR CPU route. This is workflow reachability, not an actual CI outcome.

- The PR `playground` job calls `cargo xtask playground test`. Its one owner
  ensures the pinned Modelica libraries and runs `cargo test -p rumoca-bind-wasm
  --all-features --verbose`, reaching the feature-gated guide CPU simulation and
  shader-preparation tests before its Node/WASM harness.
- PR `build-wasm` builds the optimized `release-full-web` package, then runs
  `node packages/playground/tests/user_guide_browser.mjs` in the docs shell.
  Chromium is a required Linux dependency. The driver runs book visualizers,
  FixedWing controls, interactive parameter/reset controls and all16 linked
  scenario runtime/export cells. Browser absence and mandatory CPU failures fail.
- Hardware admission may skip only Wave2D/Airfoil WebGPU execution. This does not
  skip the native compile/preparation/CPU tests or the other browser routes.
  Admitted compute failures remain failures. The admission adapter may differ
  from the actual driver device; receipts do not claim measured hardware speed.

The existing GPU-model CPU test checked only sample count. The ordinary inline
finite-sample test excludes GPU fences, so it did not close that observation gap.
The test-only followup uses the same exact guide sources and front doors. It
checks finite initialized vectors against the issued shader layout extents,
state-name extent, finite timing with positive integration steps, rectangular
finite CPU samples and the requested0/0.01/0.02 grid. Extra observations between
requested grid points remain permitted. One shared test-owned finite-vector
observer also serves the ordinary inline test without changing its prior sample
extent or endpoint requirements.

One new negative observer test exercises missing result columns, ragged vectors,
null cells, reversed times and missing endpoints/grid points, plus empty and
nonempty parameter-vector controls. The source-derived `user_guide_` count moves
from19 to20; this candidate has not run. Exact focused commands are:

```bash
cargo test --locked -p rumoca-bind-wasm --lib --features full-web user_guide_numerical_observers_reject_incomplete_or_invalid_payloads -- --nocapture
cargo test --locked -p rumoca-bind-wasm --lib --features full-web user_guide_gpu_examples_ -- --nocapture
cargo test --locked -p rumoca-bind-wasm --lib --features full-web user_guide_airfoil_aoa_slider_reseeds_the_airfoil_frame -- --nocapture
cargo test --locked -p rumoca-bind-wasm --lib --features full-web user_guide_inline_cpu_samples_are_finite -- --nocapture
```

These select1+2+1+1 tests respectively. None requires real GPU execution. Actual
browser artifact and complete-guide qualification remain pending independently.
