# Quality evidence budget ownership

Quality snapshots now carry a checked `PhaseBudgets` value with mandatory, finite, positive `model_attempt_timeout_seconds` and `sim_timeout_seconds`. The writer captures configured budgets once and uses that same value for snapshot metadata and timing-margin certification. Context comparison rejects either budget mismatch exactly. Missing fields or invalid wire values fail closed.

The checked baseline's budget metadata is corrected to its measured 20-second model / 45-second solver envelope. Its existing counts, model rosters, tolerances and provenance text are unchanged. The historical nominal CI head differs from its measured synthetic merge; the receipt records both identities.

The promoted asset has a separate reviewed correction. Only asset618765768 with SHA256 `6c6d3a77fff81fc8f328321ca1b0d0e8cf7825900e4848cd07801bb141baa4eb`, schema14/source `da9d7ca4708e6b5d694a0163d07d5007d61a86eb`, and the pinned successful CI recipe may receive the two budget-field corrections. The raw download remains intact; a separate derived file preserves every other JSON value and promoted baseline selection. Its 541 flattened / 440 compiled / 302 certified models remain its own values. Unknown legacy bytes fail closed; truthful current payloads pass through unchanged.

The existing workspace-scoped parity lock owns the derived path. The old promoted bridge and all floor/comparator policy remain unchanged. The baked historical correction values attest that immutable measurement; configured execution budgets remain owned by the harness configuration.

## Actual qualification

Source `8c657dbfc97310a007a16e078054dc3c92662e60` plus the private-helper visibility correction `169d04c47b8585d165065a37740fab85db62ce85` qualified as follows:

| Selection | Actual result |
|---|---|
| Quality unit suite |112 passed, zero failed/ignored; includes5 new PhaseBudgets controls. |
| Baseline resolver unit suite |29 passed, zero failed/ignored; includes5 real-asset controls. |
| Scoped all-target/all-feature Clippy |Passed for `rumoca-test-msl` and `xtask`. |
| Pinned workspace formatting |Passed. |

There are141 unique unit tests across the two suites. The focused5+5 runs and dirty private-helper check are repeated coverage. Tests cover the actual flattened baseline wire, constructor/wire refusals, both mismatches, exact legacy identity, full nonbudget equality, raw preservation and promoted selection.

Two earlier attempts stopped before Rust tests: importing print-dev-env's build-purity variable caused a ZeroMQ C++ build rejection; the actual developer shell then exposed the helper's excessive visibility. Their immutable receipts are retained. The correction narrows that helper, whose sole caller is local, while keeping `PhaseBudgets` private. Subsequent checks use actual `nix develop .#full`.

```sh
cargo test --locked -p rumoca-test-msl --features msl-full-test --test msl_tests balance_pipeline::balance_pipeline_quality_gate:: -- --nocapture
cargo test --locked -p xtask --bin xtask verify_cmd::msl_quality_baseline:: -- --nocapture
cargo clippy --locked -p rumoca-test-msl -p xtask --all-targets --all-features -- -D warnings
cargo fmt --all -- --check
```

[receipt.json](receipt.json) binds source guards, exact commands, logs, guardians and sealed executables. [immutable-ci-budget-proof.json](immutable-ci-budget-proof.json) binds all32 historical shard JSONs and the actual workflow hashes. Both historical CI runs’ comparators report dirty worktrees; neither is presented as clean candidate evidence.

This repairs evidence comparability. Profile selection and fresh full-cohort qualification remain separate. The prior5e6 quick result used10/12 despite baseline measurement20/45, and also exposed independent corpus/architecture failures; no overall gate or semantic closure is inferred here.

Governing owners: SPEC0018 configuration/lock channels, SPEC0029 single ownership, SPEC0021 complexity, SPEC0033§6/§6a evidence and bounded checks, and SPEC0025§4a MSL ratchets. There is no count, tolerance or model waiver.
