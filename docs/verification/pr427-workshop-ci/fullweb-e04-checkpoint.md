# Actual optimized FullWeb and guide checkpoint

Source `e04d59ad18ca422bab32e9bddb6139b22d3b305c` passed the three actual
artifact smokes and all four mandatory guide browser scripts, including all
16 authored scenario cells (14 simulation, two GALEC). The unchanged Booster
mission exercised the visible wind knob, trusted R reset, and trusted Space
launch; adjacent phase runs were exactly `[0, 1, 2, 3, 4]`. It safely landed at
simulation time 27.7 after 551 nominal 0.05-second intervals, with crash indicator
zero. The six-second burn and reset/default-input assertions passed. No exact
18-second touchdown claim is made: 18 is the authored reference duration.

The canonical optimized main/Diffsol/GALEC build completed in 329.563 seconds
on the third bounded build attempt. The preceding attempts stopped at the
unchanged host-memory reserve after 83.667 and 38.840 seconds; browser tests had
not run. Those logs and resource-stop receipts remain retained. This completion
is not a cold-build or first-attempt timing claim.

Commands ran in `nix develop .#docs`, with short scratch TMPDIR, an exclusively
owned target, four Cargo/test/Rayon workers and Chromium 154.0.8037.92. Every
build/test retained the existing 600-second, 4 GiB aggregate RSS plus swap,
8 GiB target/scratch-floor and 16 GiB host-reserve guardian. The guide wrapper
completed in 286.313 seconds, peaking at 3,472,916 KiB aggregate RSS plus swap
and 7,188,955,136 target bytes, without a resource stop.

```sh
node packages/rumoca/build.mjs --profile release --variant full-web --optimize
node packages/playground/tests/simulate_smoke.mjs --pkg-subdir release-full-web
node packages/playground/tests/source_root_smoke.mjs --pkg-subdir release-full-web
node packages/playground/tests/target_render_smoke.mjs --pkg-subdir release-full-web
node packages/playground/tests/user_guide_browser.mjs --browser-binary "$CHROMIUM"
```

The main WASM's actual `get_git_commit()` was `e04d59ad18ca`. All 24 optimized
package files (55,400,761 bytes) are sealed outside the build target. All nine
shipped runtime files matched tracked bytes, and the browser observers verified
the downloaded main/runtime/addon SHA256 values. Addons expose no Git-identity
API; their binding is the actual canonical build commands plus binary hashes.
All 4,127 tracked-source hashes and HEAD stayed identical before/after. Terminal
process audit found no visible target references, with 407 denied procfs reads
explicitly disclosed; it is not a global-idle assertion.

Both optional hardware WebGPU compute stages (Wave2D/Airfoil) skipped on the
software/fallback adapter. All mandatory CPU/software-WebGL browser scripts
passed. Those skips do not claim hardware execution or replace the independent
native preparation gates. Per-cell receipt timings are combined compilation
and runtime wall times, not isolated compile latency. Booster plant/controller
cells took 16.765/9.147 seconds; FixedWing took 10.768 seconds.

[The source-bound receipt](fullweb-e04-checkpoint.json) pins all raw log,
guardian, recipe, source-seal and package-manifest hashes. Raw artifacts are in
`$HOME/scratch/rumoca/workshop427-final-web-e04-20261012`, including the complete
552-state nominal mission trace in `actual-guide-cells.json`. The receipt SHA256
is `9b55e9fa445189da32ceb33dce1b0a92820014932baf4dfc952e9bbd746209d9`.
This is an e04 combined checkpoint, not later-tip, remote-CI, installed-wheel,
canonical quick/full or final-landing qualification.
