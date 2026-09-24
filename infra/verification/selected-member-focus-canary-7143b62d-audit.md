# 7143 focus/canary preservation vs 513621d3

Read-only fixed-cohort audit; full tier was not read. All 5 focus and 9 canary `model_worker/*/sim-trace.json` files are byte-identical to the matching 513 files, with identical raw model inventories (5/5, 9/9). Thus trace timestamps, channel names/order, values, and initial sample bytes are unchanged.

Both `msl_band_table.json` rosters and **every row payload** compare equal after removing run provenance. Focus retains 5 cohort models, 3 compared all high, 2 same exclusions; canary retains 20 roster models, 9 compared all high, 9 not attempted, 2 simulation failures. The unchanged row digests are focus `5ca72fe5d416554f5c89ae5a35f51b86a96cde306512b095b6647fd5e3c6a234` and canary `0d55c5e51fe81a9f08900a805479ff74b240164a6db184328c7505c7cd2401c3`. Compared-channel totals and scores remain focus 1,256/1,256 high and canary 175/175 high.

The trace-report initial-condition summaries are exactly equal, and no per-model initial-condition record differs: focus 1,256/1,256 initial channels high; canary 175/175 high, with unchanged mean/max, violation masses, and model counts. No builds, simulations, or profiles were run in this audit.
