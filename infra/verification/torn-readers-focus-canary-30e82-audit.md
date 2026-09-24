# Torn-reader focus/canary preservation, 30e82e22d vs ac559e1f3

Read-only comparison of paired focus and canary campaign artifacts. Source commits recorded: `30e82e22d88b20e6bba6d4d2596e6a5efc58aa25` and `ac559e1f31707d77f4e3d6a792cdf8cb6051dbd9`. The full tier is running and is outside this audit.

| Cohort | Raw/compared-high | Absent | Channels and initial channels | Rumoca trace SHA-256 manifest | OMC trace SHA-256 manifest |
|---|---:|---:|---:|---|---|
| Focus, both | 5/3 | 2 excluded; 0 missing | 1,256 high; 1,256 initial high | `757e08e6b09a7ac6970fa4489b50673470b10f52aca8140907acd899be898823` | `537aae72177e16cace34d0917846ec9b99e032a85713534a711860825e028dcc` |
| Canary, both | 9/9 | 9 not attempted, 2 failed; 0 missing | 175 high; 175 initial high | `59b7dd3c134a7e6e2ef8aa8ded4f789fc8a5d9ff926938deffb0ce038c02b64b` | `1885933faf46d33a976512ae2fb197634780671db160948bcd9ca49867fb0658` |

Each manifest is SHA-256 of sorted `relative-path\0file-SHA256\n` records. All five focus and nine canary Rumoca trace files individually hash identically to their ac559 counterparts; all corresponding OMC reference files do too. All band-table rows are field-identical across each pair. Per-model non-timing accuracy and initial-condition fields, and aggregate `trace_accuracy_stats`, are identical. `sim_trace_comparison` differs in Rumoca timing and `worst_variables` presentation: all three focus lists and all nine canary lists change order; four canary lists swap a tied-score variable at the cutoff. Every retained same-name worst-variable record is identical, and the swapped pairs have equal scores. No band or numerical-score difference follows.

One focus timing observation: Fourbar `sim_run_seconds` 7.250302956 vs 7.667870273 (−0.417567317 s, −5.446%); thyristor center-tap RLV 11.681404758 vs 11.781838804 (−0.100434046 s). These are single-run observations, with thyristor still near the 12 s limit. No build, simulation or profile was run for this audit.
