# Final ModelicaTest semantic qualification

The frozen `5e6f9cb9afec4caf992a370db78c1a5b342d5336` one-model semantic gate passed: **1 test, 0 failures, 0 ignored**, followed by successful catalog generation. The exact CI target `ModelicaTest.Blocks.MuxDemux` compiled, initialized and simulated over its authored 0–2 second horizon with 501 output points. Fresh OMC `a96aa1a-cmake` comparison measured all 46 shared channels and all 46 exact-start channels as high, with zero deviations, missing traces, skips or policy exclusions.

The independent `git archive` snapshot preserves all 4,166 Git tree entries. The regular-byte manifest has 4,165 entries because `docs/dev-guide/live` is a directory symlink; its link bytes were separately verified against the exact Git blob. All source entries and 9,382 staged library files remained unchanged. The ModelicaTest source overlay matches all 792 files at immutable pin `8ae3d35c24e519cb2996cab20f3b13daf2b0c50a`.

Raw outputs truthfully retain `git_commit: "unknown"` and `working_tree_digest: "unverified"` because this source snapshot has no Git metadata. Final source identity is bound externally by exact Git blob verification and the independently sealed final-source executable receipt. Those raw fields were not rewritten.

The supported prebuilt `xtask verify msl-parity` path used the final harness, model/simulation workers and sibling `rumoca-msl-tools`; no Cargo compilation occurred. It retained `--include-modelica-test --require-selected-targets-success --sim-set full`, the original target file and comparator requirement. There was no `--allow-unmeasured-parity`, retry, exclusion, model rewrite or tolerance change.

The guardian completed in 26.670 seconds without a resource stop. Aggregate peak resident-plus-swap memory was 2,596,516 KiB, including zero swap. The original 600-second/4-GiB memory/16-GiB host reserve/8-GiB scratch-and-cache limits and canonical per-model 10/12/14/60-second budgets were unchanged.

This qualifies the mandatory one-model semantic/parity invocation and catalog, not the whole ModelicaTest CI job. Its separate required `omc_differential_semantics::encapsulated_scope_rejection_matches_omc` step awaits a final-source `suite_core` seal. The pinned `modelica_models` compatibility commands await a final `rumoca-msl-profile` seal. Broad final gates and landing remain separate.

The concise [receipt](receipt.json) binds command, source, binary, corpus, output and resource evidence by SHA256. Full raw logs and process histories remain immutable outside the repository at the referenced `$HOME`-relative artifact paths. No cohort baseline was promoted.
