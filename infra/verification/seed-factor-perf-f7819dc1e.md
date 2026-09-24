# Bounded Fourbar seed-LU attribution

The one prepared capture completed with script exit 0, worker `sim_ok`, no lost samples, and verified frozen ELF. `capture-identity.json` binds exact main source `f7819dc1e2629876a736d3688d16e0c2ce0395a3` (tree `c4d36352b8b617d38509b26318d71cc07aa833d4`), binary SHA-256 `3113e0f89dfd1f64d84a2471d695433d412208cd949e1bdc849d102bf72b159d`, prior 6fb request with only `output_dir` changed, build/runtime flags, and worker PID 3598263. The certified Sim-only interval `206466.5–206472` is inside observed Sim `206466.039151346–206472.648702132`; its 5.507 s counter interval had 5.14 user + 0.32 system seconds, 138,880 minor and zero major faults.

Counting CPU-clock sample paragraphs in `stacks-sim.txt` containing the dynamic nalgebra `lu<f64, nalgebra::base::dimension::Dyn` symbol and, separately, a `seed_linearization` frame gives:

| Capture | CPU samples | Dynamic-LU stacks | Seed-attributed LU stacks | Dynamic-LU report self |
|---|---:|---:|---:|---:|
| 6b5e prior | 1,123 | 100 | 97 | 8.82% |
| f781 current | 1,041 | 3 | 0 | 0.29% |

The old seed LU hotspot is absent at this sample resolution; the three remaining dynamic-LU stacks do not contain a seed-linearization frame. This corroborates the factor-only source change's intended effect on Fourbar. Instrumented windows differ (old 6 s, new 5.5 s), and sampling cannot prove zero seed factors or quantify canonical speedup. This Fourbar observation says nothing about the Thyristor timeout's cause. No further capture, probe, or optimization followed.
