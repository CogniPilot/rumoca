# MSL Wall-Time Trust Gate Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Keep MSL wall-time regression blocking only for fresh, affinity-correct, healthy-host measurements while preserving every existing correctness and system-time gate.

**Architecture:** Extend the worker handshake so scheduler metrics describe actual affinity, add a small MSL measurement-provenance module that records cache and host-health evidence, then make the quality gate classify wall-time as `PASS`, `FAIL`, or `ADVISORY`. The runtime tolerance remains unchanged; only the trust qualification changes whether a wall regression is blocking.

**Tech Stack:** Rust 2024, serde/serde_json, Cargo integration tests, Rumoca model-worker protocol, MSL parity harness.

## Global Constraints

- Keep the existing runtime regression tolerance exactly `0.35`.
- System-time runtime regression remains blocking on every complete parity run.
- Stage-count, balance, simulation, trace, OMC availability, runtime-sample presence, and target-set checks remain fail-closed.
- Wall time is blocking only when every compared OMC runtime sample is fresh, requested Rumoca worker affinity succeeded, normalized one-minute load is present and `<= 1.5` before and after measured work, and runtime worker/thread context matches policy.
- Missing or malformed wall-time trust provenance makes only the wall regression advisory; it must never make missing parity data valid.
- Do not change or promote `msl_quality_baseline.json`.
- Do not add retry loops, bypass environment variables, fitted constants, or generated-artifact patches.
- Update `SPEC_0025_PR_REVIEW_PROCESS.md` and `docs/dev-guide/src/tooling/msl-quality-gate.md` with the final policy.
- Commit and push only to the ClimaMind `origin` feature branch `cli-52-upstream-equivalent-cleanup`; never push or open a PR against `upstream`.

---

### Task 1: Report Actual Worker Affinity

**Files:**
- Modify: `crates/rumoca-worker/src/lib.rs`
- Modify: `crates/rumoca-worker/src/bin/rumoca-worker.rs`
- Modify: `crates/rumoca-test-msl/tests/balance_pipeline/mod.rs`
- Modify: `crates/rumoca-test-msl/tests/balance_pipeline/balance_pipeline_core.rs`
- Modify: `crates/rumoca-test-msl/tests/balance_pipeline/balance_pipeline_core/streaming_workers.rs`
- Test: `crates/rumoca-worker/src/lib.rs`
- Test: `crates/rumoca-test-msl/tests/balance_pipeline/balance_pipeline_core/tests.rs`

**Interfaces:**
- Produces: `ModelWorkerControlMessage::Ready { protocol_version, cpu_affinity_applied: Option<bool> }`.
- Produces: `ModelWorkerDaemon::cpu_affinity_applied(&self) -> Option<bool>`.
- Produces: scheduler counts `affinity_requested_worker_count`, `affinity_applied_worker_count`, and `affinity_failed_worker_count`; `pinned_worker_count` remains serialized for compatibility but equals actual affinity successes.
- Consumes: existing `pin_current_thread_to_cpu_core` and `cpu_core_plan`.

- [ ] **Step 1: Write failing worker-protocol tests**

Add tests that serialize and deserialize both requested-affinity success and no-request cases:

```rust
#[test]
fn ready_message_preserves_affinity_result() {
    let ready = ModelWorkerControlMessage::Ready {
        protocol_version: MODEL_WORKER_PROTOCOL_VERSION,
        cpu_affinity_applied: Some(false),
    };
    let encoded = serde_json::to_string(&ready).unwrap();
    let decoded: ModelWorkerControlMessage = serde_json::from_str(&encoded).unwrap();
    assert!(matches!(
        decoded,
        ModelWorkerControlMessage::Ready {
            cpu_affinity_applied: Some(false),
            ..
        }
    ));
}

#[test]
fn ready_message_allows_unrequested_affinity() {
    let ready = ModelWorkerControlMessage::Ready {
        protocol_version: MODEL_WORKER_PROTOCOL_VERSION,
        cpu_affinity_applied: None,
    };
    assert!(serde_json::to_string(&ready).unwrap().contains("cpu_affinity_applied"));
}
```

- [ ] **Step 2: Run RED test**

Run:

```bash
CARGO_TARGET_DIR=/Users/hechuan/workspace/climamind/rumoca/target \
  cargo test -p rumoca-worker ready_message_ -- --nocapture
```

Expected: compilation fails because `Ready` has no `cpu_affinity_applied` field.

- [ ] **Step 3: Implement the handshake and daemon accessor**

Compute affinity once in `run_worker_entry`:

```rust
let cpu_affinity_applied = args.cpu_core_id.map(|cpu_core_id| {
    pin_current_thread_to_cpu_core(cpu_core_id)
        .inspect_err(|error| eprintln!("warning: {error}; continuing without CPU pinning"))
        .is_ok()
});
```

Pass the value to `run_worker_daemon`, include it in `Ready`, retain it in
`ModelWorkerDaemon::wait_for_ready`, and expose it through the accessor. Keep
`None` for workers for which affinity was not requested.

- [ ] **Step 4: Write failing scheduler aggregation tests**

Add a pure aggregation helper and tests:

```rust
#[test]
fn affinity_counts_distinguish_requested_success_and_failure() {
    let counts = affinity_counts([Some(true), Some(false), None]);
    assert_eq!(counts.requested, 2);
    assert_eq!(counts.applied, 1);
    assert_eq!(counts.failed, 1);
}
```

Run:

```bash
CARGO_TARGET_DIR=/Users/hechuan/workspace/climamind/rumoca/target \
  cargo test -p rumoca-test-msl --features msl-full-test --test msl_tests \
  affinity_counts_ -- --nocapture
```

Expected: FAIL because the aggregation helper and fields do not exist.

- [ ] **Step 5: Aggregate actual results**

Record the daemon accessor value once per spawned worker. Extend
`SchedulerStatsCollector` with atomics for requested, applied, and failed
affinity. Populate `MslSchedulerTimings` from those atomics, and set
`pinned_worker_count = affinity_applied_worker_count`. Do not count planned
core IDs as successful pinning.

- [ ] **Step 6: Run GREEN tests and affected package tests**

Run:

```bash
CARGO_TARGET_DIR=/Users/hechuan/workspace/climamind/rumoca/target \
  cargo test -p rumoca-worker -- --nocapture
CARGO_TARGET_DIR=/Users/hechuan/workspace/climamind/rumoca/target \
  cargo test -p rumoca-test-msl --features msl-full-test --test msl_tests \
  affinity_ -- --nocapture
```

Expected: all selected tests pass with no warnings from test code.

- [ ] **Step 7: Commit**

```bash
git add crates/rumoca-worker/src/lib.rs \
  crates/rumoca-worker/src/bin/rumoca-worker.rs \
  crates/rumoca-test-msl/tests/balance_pipeline/mod.rs \
  crates/rumoca-test-msl/tests/balance_pipeline/balance_pipeline_core.rs \
  crates/rumoca-test-msl/tests/balance_pipeline/balance_pipeline_core/streaming_workers.rs \
  crates/rumoca-test-msl/tests/balance_pipeline/balance_pipeline_core/tests.rs
git commit -s -m "fix(msl): report actual worker affinity"
```

---

### Task 2: Capture Wall-Time Measurement Provenance

**Files:**
- Create: `crates/rumoca-test-msl/tests/balance_pipeline/runtime_measurement.rs`
- Modify: `crates/rumoca-test-msl/tests/balance_pipeline/mod.rs`
- Modify: `crates/rumoca-test-msl/tests/balance_pipeline/balance_pipeline_core.rs`
- Modify: `crates/rumoca-test-msl/tests/balance_pipeline/balance_pipeline_summary.rs`
- Modify: `crates/rumoca-test-msl/src/msl_tools/omc_simulation_reference.rs`
- Modify: `crates/rumoca-test-msl/src/msl_tools/omc_simulation_reference/output.rs`
- Test: `crates/rumoca-test-msl/tests/balance_pipeline/runtime_measurement.rs`
- Test: `crates/rumoca-test-msl/src/msl_tools/omc_simulation_reference/tests.rs`

**Interfaces:**
- Consumes: Task 1 scheduler affinity counts serialized in `msl_results.json`.
- Produces: `HostLoadSnapshot { one_minute: f64, logical_cpus: usize }` with `normalized() -> f64`.
- Produces: `WallTimeMeasurementProvenance` serialized under `runtime_comparison.wall_time_provenance`.
- Produces fields: `omc_fresh_sample_count`, `omc_cached_sample_count`, `affinity_requested_worker_count`, `affinity_applied_worker_count`, `affinity_failed_worker_count`, `normalized_load_before`, `normalized_load_after`, `workers_used`, and `omc_threads`.

- [ ] **Step 1: Write failing host-load parsing and threshold tests**

Create the module with tests first:

```rust
#[test]
fn parses_linux_loadavg() {
    let snapshot = parse_load_text("15.0 8.0 4.0 2/100 123", 10).unwrap();
    assert_eq!(snapshot.one_minute, 15.0);
    assert_eq!(snapshot.normalized(), 1.5);
}

#[test]
fn parses_macos_vm_loadavg() {
    let snapshot = parse_load_text("{ 2.50 3.00 4.00 }", 10).unwrap();
    assert_eq!(snapshot.one_minute, 2.5);
    assert_eq!(snapshot.normalized(), 0.25);
}

#[test]
fn rejects_missing_or_non_finite_load() {
    assert!(parse_load_text("unavailable", 10).is_none());
    assert!(parse_load_text("NaN 1 1", 10).is_none());
    assert!(parse_load_text("1 1 1", 0).is_none());
}
```

- [ ] **Step 2: Run RED load tests**

Run:

```bash
CARGO_TARGET_DIR=/Users/hechuan/workspace/climamind/rumoca/target \
  cargo test -p rumoca-test-msl --features msl-full-test --test msl_tests \
  runtime_measurement -- --nocapture
```

Expected: FAIL because the module and parser do not exist.

- [ ] **Step 3: Implement safe platform load sampling**

Implement `parse_load_text` without `unsafe`. On Linux read `/proc/loadavg`;
on macOS execute `sysctl -n vm.loadavg`; on other platforms return `None`.
Use `std::thread::available_parallelism()` for the denominator. Sample at the
beginning and end of `run_msl_test` and serialize both snapshots in
`MslPhaseTimings` with `#[serde(default)]` compatibility.

- [ ] **Step 4: Write failing OMC cache-provenance tests**

Extend the existing resume fixtures so one cached result and one newly run
result produce exact counts:

```rust
assert_eq!(payload["runtime_comparison"]["wall_time_provenance"]["omc_cached_sample_count"], 1);
assert_eq!(payload["runtime_comparison"]["wall_time_provenance"]["omc_fresh_sample_count"], 1);
```

Also assert that affinity and load values from the Rumoca result summary are
copied into the same provenance object.

- [ ] **Step 5: Run RED provenance tests**

Run:

```bash
CARGO_TARGET_DIR=/Users/hechuan/workspace/climamind/rumoca/target \
  cargo test -p rumoca-test-msl omc_simulation_reference -- --nocapture
```

Expected: FAIL because `wall_time_provenance` is absent.

- [ ] **Step 6: Track cache origin and emit provenance**

Extend `SimRunState` with a set of cached OMC model names populated by
`merge_cached_results_for_resume`. At output time, count only models that
participate in `wall_ratio_both_success`; classify each as fresh or cached.
Read scheduler affinity and host-load snapshots from `msl_results.json` and
write the structured provenance alongside the runtime ratio stats. Do not
infer freshness from `batches_skipped` alone.

- [ ] **Step 7: Run GREEN tests**

Run both commands from Steps 2 and 5. Expected: all selected tests pass and
the provenance counts exactly match their fixtures.

- [ ] **Step 8: Commit**

```bash
git add crates/rumoca-test-msl/tests/balance_pipeline/runtime_measurement.rs \
  crates/rumoca-test-msl/tests/balance_pipeline/mod.rs \
  crates/rumoca-test-msl/tests/balance_pipeline/balance_pipeline_core.rs \
  crates/rumoca-test-msl/tests/balance_pipeline/balance_pipeline_summary.rs \
  crates/rumoca-test-msl/src/msl_tools/omc_simulation_reference.rs \
  crates/rumoca-test-msl/src/msl_tools/omc_simulation_reference/output.rs \
  crates/rumoca-test-msl/src/msl_tools/omc_simulation_reference/tests.rs
git commit -s -m "feat(msl): record wall-time measurement provenance"
```

---

### Task 3: Enforce Trust-Qualified Wall-Time Policy

**Files:**
- Modify: `crates/rumoca-test-msl/tests/balance_pipeline/balance_pipeline_quality_gate.rs`
- Modify: `crates/rumoca-test-msl/tests/balance_pipeline/balance_pipeline_quality_gate/status.rs`
- Modify: `crates/rumoca-test-msl/tests/balance_pipeline/balance_pipeline_quality_gate/tests.rs`
- Modify: `spec/SPEC_0025_PR_REVIEW_PROCESS.md`
- Modify: `docs/dev-guide/src/tooling/msl-quality-gate.md`

**Interfaces:**
- Consumes: Task 2 `runtime_comparison.wall_time_provenance`.
- Produces: `WallTimeTrustDecision { trusted: bool, reasons: Vec<String> }`.
- Produces: blocking reasons containing wall regression only when `trusted` is true.
- Produces: console status `MSL wall speed gate: PASS|FAIL|ADVISORY` with provenance reasons.

- [ ] **Step 1: Write failing trust-decision tests**

Add fixtures and tests for all policy branches:

```rust
#[test]
fn cached_omc_wall_regression_is_advisory_but_system_regression_blocks() {
    let parity = parity_with_provenance(
        runtime_ratio_stats(1.0, 0.5),
        provenance(/* fresh */ 0, /* cached */ 10, 2, 2, 0, 0.2, 0.3),
    );
    let mut reasons = Vec::new();
    push_runtime_ratio_regression_reasons(&mut reasons, &baseline_with_runtime(2.0, 1.5), Some(&parity));
    assert!(reasons.iter().any(|reason| reason.contains("runtime system speedup median")));
    assert!(!reasons.iter().any(|reason| reason.contains("runtime wall speedup median")));
    assert!(wall_time_trust_decision(Some(&parity)).reasons.iter().any(|reason| reason.contains("cached")));
}

#[test]
fn trusted_wall_regression_remains_blocking() {
    let parity = parity_with_provenance(
        runtime_ratio_stats(2.0, 0.5),
        provenance(10, 0, 2, 2, 0, 0.5, 0.6),
    );
    let mut reasons = Vec::new();
    push_runtime_ratio_regression_reasons(&mut reasons, &baseline_with_runtime(2.0, 1.5), Some(&parity));
    assert!(reasons.iter().any(|reason| reason.contains("runtime wall speedup median")));
}
```

Add separate tests proving affinity failure, load `> 1.5`, missing load, missing
provenance, and mismatched worker/thread policy each produce `trusted == false`.

- [ ] **Step 2: Run RED gate tests**

Run:

```bash
CARGO_TARGET_DIR=/Users/hechuan/workspace/climamind/rumoca/target \
  cargo test -p rumoca-test-msl --features msl-full-test --test msl_tests \
  wall_time_ -- --nocapture
```

Expected: FAIL because the trust model and advisory behavior do not exist.

- [ ] **Step 3: Implement parsing and pure trust decision**

Add serde-compatible provenance types and parse the Task 2 JSON object. The
pure decision must append stable reason strings for cached samples, affinity
failure, missing/excessive load, and runtime-context mismatch. `trusted` is
true only when the reason list is empty.

- [ ] **Step 4: Apply trust decision to blocking reasons and status**

Keep the existing unconditional system comparison. Wrap only the wall
regression insertion with `wall_time_trust_decision(...).trusted`. Update
status output so advisory measurements print their observed median, baseline,
35% floor, and reason list without saying `PASS`.

- [ ] **Step 5: Update policy documentation**

Change the runtime row in `SPEC_0025_PR_REVIEW_PROCESS.md` to state:

```text
Runtime system-time speedup median MUST NOT regress by > 35%. Wall-time uses
the same 35% limit only for fresh, affinity-correct, healthy-host paired
measurements; otherwise it remains visible as ADVISORY and does not mask any
correctness or system-time failure.
```

Add the same behavior, provenance fields, and `PASS|FAIL|ADVISORY` meanings to
the MSL quality-gate developer guide.

- [ ] **Step 6: Run focused GREEN tests**

Run:

```bash
CARGO_TARGET_DIR=/Users/hechuan/workspace/climamind/rumoca/target \
  cargo test -p rumoca-test-msl --features msl-full-test --test msl_tests \
  runtime_ratio_ -- --nocapture
CARGO_TARGET_DIR=/Users/hechuan/workspace/climamind/rumoca/target \
  cargo test -p rumoca-test-msl --features msl-full-test --test msl_tests \
  wall_time_ -- --nocapture
```

Expected: every selected runtime and trust test passes.

- [ ] **Step 7: Run affected verification**

Run:

```bash
cargo fmt --check
CARGO_TARGET_DIR=/Users/hechuan/workspace/climamind/rumoca/target \
  cargo clippy -p rumoca-worker -p rumoca-test-msl --all-targets --all-features -- -D warnings
CARGO_TARGET_DIR=/Users/hechuan/workspace/climamind/rumoca/target \
  cargo test -p rumoca-worker
CARGO_TARGET_DIR=/Users/hechuan/workspace/climamind/rumoca/target \
  cargo test -p rumoca-test-msl --features msl-full-test --test msl_tests \
  balance_pipeline::balance_pipeline_quality_gate::tests -- --nocapture
CARGO_TARGET_DIR=/Users/hechuan/workspace/climamind/rumoca/target \
  cargo test -p rumoca --test architecture_hardening_test
cargo xtask verify docs
```

Expected: all commands exit 0.

- [ ] **Step 8: Run the real MSL parity acceptance gate**

Run:

```bash
CARGO_TARGET_DIR=/Users/hechuan/workspace/climamind/rumoca/target \
  cargo xtask verify msl-parity
```

Expected on the current cached/high-load path: correctness and system-time
checks pass; a wall regression prints `ADVISORY` with cache, affinity, or load
reasons and does not fail the run. Do not claim a trusted wall-time pass unless
the emitted provenance actually satisfies every trust condition.

- [ ] **Step 9: Commit**

```bash
git add crates/rumoca-test-msl/tests/balance_pipeline/balance_pipeline_quality_gate.rs \
  crates/rumoca-test-msl/tests/balance_pipeline/balance_pipeline_quality_gate/status.rs \
  crates/rumoca-test-msl/tests/balance_pipeline/balance_pipeline_quality_gate/tests.rs \
  spec/SPEC_0025_PR_REVIEW_PROCESS.md \
  docs/dev-guide/src/tooling/msl-quality-gate.md
git commit -s -m "fix(msl): trust-qualify wall-time regressions"
```

---

## Final Branch Verification and Delivery

- [ ] Generate a whole-branch review package from merge base `6936a2043843ac701cc0cfac57bef61ba9de9a68` to final HEAD and dispatch the final code reviewer.
- [ ] Resolve every Critical or Important finding through one fix subagent and re-review.
- [ ] Re-run the affected verification commands after the final fix commit.
- [ ] Confirm `git status --short` is empty and every commit has `Signed-off-by`.
- [ ] Push only:

```bash
git push origin cli-52-upstream-equivalent-cleanup
```

- [ ] Fetch and prove local branch, `origin/cli-52-upstream-equivalent-cleanup`, and the intended final SHA are equal.
- [ ] Confirm no push or pull request was made to the `upstream` remote.
