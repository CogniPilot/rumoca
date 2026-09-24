# Compact Jacobian focused and canary preservation audit

Read-only comparison of `paired-preservation-focused-ac559e1f3` and `canary-ac559e1f3` against their `3b7773263` paired baselines. No builds or profiles.

- Focused: 5/5 construction and simulation successes, 3/3 compared high, 2/2 same policy exclusions, 0 missing. All 1,256 compared channels and all 1,256 initial channels remain high. Per-model bands/counts/scores, band-row digest, and non-timing model results are identical. All five Rumoca `sim-trace.json` files and five OMC reference trace files are byte-identical by SHA-256.
- Canary: 9/9 compared high, 175/175 channels high, 175/175 initial channels high, 0 excluded/missing. The eleven other roster rows retain their exact earlier exit reasons. Band-row digest and non-timing model results are identical; all nine Rumoca and nine OMC trace files are byte-identical by SHA-256.
- Focused simulation **run** time: Fourbar 7.570→7.668 s (+1.3%), Thyristor 9.927→11.782 s (+18.7%), RollingWheel 0.952→0.654 s (−31.3%). Fourbar construction compile 1.926→1.946 s; its separate simulation build 10.206→9.878 s. These are single paired observations, not a speedup claim. Other focused run times: SpringWithMass 0.103→0.128 s; DemoPowerSupplyWithBuffer 2.564→2.514 s.

Full `tier2-compact-ac559e1f3` was still running at this audit; no full-tier artifacts were read.
