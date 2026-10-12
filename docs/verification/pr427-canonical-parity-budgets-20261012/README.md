# Canonical MSL parity measurement budgets

The canonical quick/full parity step omitted timeout configuration, selecting the focused harness defaults (10 seconds per worker phase and 12 seconds per simulation). CI shards and merge explicitly used 20/45 seconds. SPEC_0050 timing-margin certification compares actual phase envelopes; these invocations were not comparable.

The typed `--budget-profile ci` now supplies one 20/45 definition to quick, full, CI shards and CI merge. Explicit timeout flags conflict with the profile at argument parsing. Unprofiled defaults and explicit nightly/isolated diagnostic overrides remain supported. Compiler behavior, model selection, baseline floors and numerical tolerances are unchanged.

Actual BEFORE source `227dc1b88c8818d1e7294ed612d3e74d06248a1f` is an exact `5e6f9cb9afec4caf992a370db78c1a5b342d5336` descendant with only the 24-line regression added. All production invocation/CI owner bytes match that base. The real canonical CLI/config test failed with absent simulation budget versus expected 45 seconds (0 passed, 1 failed); source seals match and the guardian did not stop it.

At frozen AFTER `e5035ae343bc12d1fdccb9bea5e9ee2e49e5cc6b`, eight tests pass: five budget controls, canonical quick/full parsing into actual config, and both existing suite rosters. Scoped xtask all-target/all-feature Clippy and workspace formatting pass. Both source seals match. The [receipt](qualification.json) pins the commands, raw logs, guardians, source manifests, owner-byte equivalence and sealed AFTER test ELF. The BEFORE ELF was not separately retained; its actual invocation is recorded in the raw log.

This qualifies the shared invocation owner. Combined-source budget metadata and actual canonical corpus parity remain separate requirements; no green CI or full MSL claim follows from these focused controls.

The [historical CI budget receipt](historical-ci-budget-metadata.json), supplied by the ROOT evidence owner, pins two immutable workflow sources and their shard artifacts. All 34 workflow/artifact hashes were independently checked. Both runs issued 20/45 budgets. The first run's actual artifact source is synthetic merge `4dc555662188ef8e42075d0b4100ed7205492724`, distinct from its nominal head; the second measures `da9d7ca4708e6b5d694a0163d07d5007d61a86eb`. Historical comparator artifacts disclose dirty worktrees. These records establish the invocation envelope, not a clean final-candidate result. Their promoted asset's stored 12-second metadata is separately corrected by the budget-evidence owner.
