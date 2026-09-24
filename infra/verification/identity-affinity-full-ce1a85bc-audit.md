# Full identity-affinity audit: ce1a85bc vs be97aace

Read-only comparison of terminal `/tmp/rumoca-fluid/target/fluid-campaign/tier2-identity-affinity-ce1a85bc` (reported exit 1) with `tier2-provenance-be97aace`. Source `ce1a85bc9785e3ba9eda10249d1c8316ea8a472d`; both have the exact same 566-model roster, 211 successful-simulation roster, 192 compared-model roster, 19 policy-exclusion roster and reasons, and 211 byte-identical OMC reference traces. No missing comparison trace. The band roster is exactly 192 high, zero near/deviation; the 35,961 compared channels retain 35,919 high and 42 minor, with zero deviation/severe; all 35,961 initial channels remain high.

Of 211 Rumoca raw simulation traces, 208 are byte-identical. The only SHA-256 changes are:

| Model | be97 | ce1 |
|---|---|---|
| PlanarFourbar | `863c9039b40734fcdb1e2f3eb411bfdb4090bf171e6e576b2f0d2f88faf1c3b7` | `bc3001c2a6b01130b304d3f9a73ceaa6c248c3c08bccfb0d8f7002f909bc885f` |
| Engine1b | `649d47c57647194c204b99cbfd9f82dd21bcdad2cc229f8c5e46ce4b673d02a0` | `9dff8a8f745d5c0cd764708c9ad0a8d0c719af14572d8a33352bc87386e64efb` |
| Fourbar1 | `7f0ebb08c690e4654500d632becfd2a34c53dde069ead992d3cfd3b15ee40315` | `1a5edad3539e82a4989ba09b4dd3a204500ffe56a06be36eb05a8a5406231e6f` |

Across all 192 compared model score rows, only seven scalar cells change; no channel count, sample count, violation mass, or band changes:

| Model | Score | be97 → ce1 |
|---|---|---|
| Engine1b | max channel bounded normalized L1 | `0.000760693075130334` → `0.000760693382975177` |
| Engine1b | mean channel bounded normalized L1 | `2.78621173226921e-05` → `2.78665945305626e-05` |
| Fourbar1 | model bounded normalized L1 | `1.20257829800408e-11` → `9.97948709377725e-12` |
| Fourbar1 | max channel bounded normalized L1 | `0.000138260355181057` → `0.000131133762936449` |
| Fourbar1 | mean channel bounded normalized L1 | `2.89153769948066e-05` → `2.74684262404335e-05` |
| PlanarFourbar | max channel bounded normalized L1 | `6.90383023635665e-05` → `6.78964103540756e-05` |
| PlanarFourbar | mean channel bounded normalized L1 | `1.20480329181832e-05` → `1.1849973490903e-05` |

Initial-condition records are exactly unchanged for 191/192 models. Engine1b's mean channel bounded normalized error changes `2.26646372192071e-17` → `2.26646372165004e-17`; its 1,293 initial channels remain high, with no minor/deviation/severe channel. Some `worst_variables` top-ten lists select different ties even for byte-identical traces; the authoritative scalar score/band rows and raw traces above are the comparison.

Failure/exit classification: same 307 compiled, 288 simulation attempts, 232 IC successes, 211 simulation successes, 77 simulation failures, 19 excluded and 278 not attempted. The reported exit 1 remains a non-parity quality-gate result; no numerical parity loss appears. `Inverse_sh_TX` progresses from Flatten failure `EF015` (missing resolved record metadata) to ToDae failure `ED019` (parameter-binding type mismatch); this shifts flatten success 527→528 and ToDae failure 220→221. `PsychrometricData` remains a ToDae shape-proof failure, but its diagnostic required extent changes 2→1. Timeout elapsed text drifts in otherwise unchanged failure categories. These diagnostic changes are disclosed, not attributed to the identity proof.

Full-run Fourbar `sim_run_seconds` is `6.713594141` → `6.588155208`, while `sim_build_seconds` is `10.431087426` → `10.465830302`; these are one-observation timings, not causal attribution. Disposition: exact completion/high-band preservation with three numerical traces changed inside the same high bands; no actionable numerical counterexample. Full quality gate still exits 1 on the retained cohort floors.
