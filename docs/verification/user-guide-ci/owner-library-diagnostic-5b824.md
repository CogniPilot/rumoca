# Measured owner-library diagnostic at 5b824

Source `5b82462ea2b8a38530dca688a45c0cf1e08204cc` measured **1,086 passed / 0 failed / 0 ignored** across three exact-count gates: all 215 DAE library tests; 608 of 609 Solve library tests; and 263 of 264 structural library tests. This is a bounded diagnostic, **not full qualification**: two known original tests were explicitly excluded.

Solve excluded only `tests::representative_solve_problem_json_matches_committed_golden`, whose committed golden required current-wire producer refresh. Structural excluded only `nested_model_arm_function_interval_survives_structural_replay`, whose issued guard operations required the independently owned producer repair. These exclusions are visible in the actual commands and filtered counts; they are not waivers. Both originals and all full libraries must qualify on the final source. The DAE count includes the actual property-test driver as one Rust test.

[Portable receipt](owner-library-diagnostic-5b824.json) retains actual summaries, exact commands, raw log/guardian/count hashes, recipe/source identity, runner binding and unchanged limits. All children exited zero, with no signal or resource termination. Terminal audit observed zero owned-target references and disclosed 2275 denied reads as unobserved; the runner released its lease. No source-count estimate, skipped test or historical result is counted as a final-tip pass.

The subsequent 41b2 structural/root/guide qualification is separate and remains pending in this receipt. Final quick/full verification, source-built typed-call fixture issuance, instrumented coverage, remote CI and issue closure are unclaimed.
