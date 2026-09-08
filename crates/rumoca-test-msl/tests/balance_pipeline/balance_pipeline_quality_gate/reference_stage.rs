//! The OMC reference + trace-comparison stage of the MSL parity gate.
//!
//! Split out of `balance_pipeline_quality_gate.rs` so the stage that PRODUCES a
//! parity reading lives apart from the gate that JUDGES one. Every exit from
//! this module is typed: see `ensure_required_msl_parity_references` for the
//! acceptance contract.

use super::*;

const TRACE_COMPARISON_FILE_REL: &str = "sim_trace_comparison.json";

/// Opaque evidence that this invocation produced one non-vacuous trace
/// comparison payload.
///
/// The payload is read exactly once, immediately after the producing command
/// succeeds. Downstream report writers can only borrow this receipt; they do not
/// rediscover a similarly named file in a reused results directory.
#[derive(Debug)]
pub(crate) struct CurrentRunTraceComparison {
    validated: rumoca_test_msl::msl_tools::omc_simulation_reference::ValidatedTraceReport,
    cohort: std::sync::OnceLock<Result<MslCohortReading, MslParityUnmeasuredReason>>,
}

impl CurrentRunTraceComparison {
    pub(crate) fn payload(&self) -> &serde_json::Value {
        self.validated.payload()
    }

    pub(crate) fn exact_digest(&self) -> &str {
        self.validated.report_digest()
    }

    pub(crate) fn source_evidence_digest(&self) -> &str {
        self.validated.source_evidence_digest()
    }

    pub(crate) fn reference_payload(&self) -> &serde_json::Value {
        self.validated.reference_payload()
    }

    pub(crate) fn reference_digest(&self) -> &str {
        self.validated.reference_digest()
    }

    pub(crate) fn cohort_reading(
        &self,
        initialize: impl FnOnce() -> Result<MslCohortReading, MslParityUnmeasuredReason>,
    ) -> Result<MslCohortReading, MslParityUnmeasuredReason> {
        self.cohort.get_or_init(initialize).clone()
    }

    pub(crate) fn validated(
        &self,
    ) -> &rumoca_test_msl::msl_tools::omc_simulation_reference::ValidatedTraceReport {
        &self.validated
    }
}

impl PartialEq for CurrentRunTraceComparison {
    fn eq(&self, other: &Self) -> bool {
        self.payload() == other.payload()
            && self.reference_payload() == other.reference_payload()
            && self.exact_digest() == other.exact_digest()
            && self.reference_digest() == other.reference_digest()
            && self.source_evidence_digest() == other.source_evidence_digest()
    }
}

impl Eq for CurrentRunTraceComparison {}

fn checked_trace_comparison_payload(
    validated: rumoca_test_msl::msl_tools::omc_simulation_reference::ValidatedTraceReport,
    source: &Path,
) -> io::Result<CurrentRunTraceComparison> {
    let payload = validated.payload();
    ensure_trace_comparison_nonvacuous(payload, source)?;
    for (label, digest) in [
        ("report", validated.report_digest()),
        ("reference", validated.reference_digest()),
        ("source-evidence", validated.source_evidence_digest()),
    ] {
        if digest.len() != 64
            || !digest
                .bytes()
                .all(|byte| byte.is_ascii_digit() || (b'a'..=b'f').contains(&byte))
        {
            return Err(io::Error::other(format!(
                "current trace comparison '{}' has invalid {label} digest",
                source.display()
            )));
        }
    }
    Ok(CurrentRunTraceComparison {
        validated,
        cohort: std::sync::OnceLock::new(),
    })
}

fn ensure_trace_comparison_nonvacuous(
    payload: &serde_json::Value,
    source: &Path,
) -> io::Result<()> {
    let models = payload
        .get("models")
        .and_then(serde_json::Value::as_object)
        .ok_or_else(|| {
            io::Error::other(format!(
                "current trace comparison '{}' has no models object",
                source.display()
            ))
        })?;
    let declared = payload
        .get("models_compared")
        .and_then(serde_json::Value::as_u64)
        .and_then(|count| usize::try_from(count).ok())
        .ok_or_else(|| {
            io::Error::other(format!(
                "current trace comparison '{}' has no valid models_compared",
                source.display()
            ))
        })?;
    if declared == 0 || declared != models.len() {
        return Err(io::Error::other(format!(
            "current trace comparison '{}' is vacuous or inconsistent: models_compared={declared}, model records={}",
            source.display(),
            models.len()
        )));
    }
    Ok(())
}

fn invalidate_trace_comparison_at(path: &Path) -> io::Result<()> {
    match fs::remove_file(path) {
        Ok(()) => {}
        Err(error) if error.kind() == io::ErrorKind::NotFound => {}
        Err(error) => {
            return Err(io::Error::other(format!(
                "failed to invalidate previous trace comparison '{}': {error}",
                path.display()
            )));
        }
    }
    Ok(())
}

/// Exclusive output slot for the comparator subprocess.
///
/// Claiming the slot destroys any previous artifact. Until `issue` validates
/// the new payload, every exit removes partial output as well. A failed command
/// therefore cannot leave bytes that a later process mistakes for this run's
/// comparison.
struct CurrentTraceComparisonOutput {
    path: PathBuf,
    issued: bool,
}

impl CurrentTraceComparisonOutput {
    fn claim() -> io::Result<Self> {
        Self::claim_at(msl_results_dir().join(TRACE_COMPARISON_FILE_REL))
    }

    fn claim_at(path: PathBuf) -> io::Result<Self> {
        invalidate_trace_comparison_at(&path)?;
        Ok(Self {
            path,
            issued: false,
        })
    }

    fn issue(mut self) -> io::Result<CurrentRunTraceComparison> {
        let receipt = read_current_trace_comparison(&self.path)?;
        self.issued = true;
        Ok(receipt)
    }
}

impl Drop for CurrentTraceComparisonOutput {
    fn drop(&mut self) {
        if self.issued {
            return;
        }
        if let Err(error) = invalidate_trace_comparison_at(&self.path) {
            eprintln!(
                "failed to remove unissued trace comparison '{}': {error}",
                self.path.display()
            );
        }
    }
}

fn read_current_trace_comparison(path: &Path) -> io::Result<CurrentRunTraceComparison> {
    let bytes = fs::read(path).map_err(|error| {
        io::Error::other(format!(
            "comparator command returned success without producing '{}': {error}",
            path.display()
        ))
    })?;
    let reference_path = path
        .parent()
        .unwrap_or_else(|| Path::new("."))
        .join(OMC_SIM_REFERENCE_FILE_REL);
    let reference_bytes = fs::read(&reference_path).map_err(|error| {
        io::Error::other(format!(
            "current trace comparison '{}' has no exact OMC reference '{}': {error}",
            path.display(),
            reference_path.display()
        ))
    })?;
    let paths = rumoca_test_msl::msl_tools::common::MslPaths::current()
        .with_results_dir(path.parent().unwrap_or_else(|| Path::new(".")));
    let validated = rumoca_test_msl::msl_tools::omc_simulation_reference::validate_trace_report_against_sources(
        &paths,
        &bytes,
        &reference_bytes,
    )
    .map_err(|error| {
        io::Error::other(format!(
            "current trace comparison '{}' is not bound to its source traces: {error:#}",
            path.display()
        ))
    })?;
    checked_trace_comparison_payload(validated, path)
}

pub(crate) fn merged_trace_comparison_receipt(
    results_dir: &Path,
    report_bytes: &[u8],
    reference_bytes: &[u8],
    shard_witnesses: &[rumoca_test_msl::msl_tools::omc_simulation_reference::ValidatedTraceReport],
) -> Result<CurrentRunTraceComparison, String> {
    let paths =
        rumoca_test_msl::msl_tools::common::MslPaths::current().with_results_dir(results_dir);
    let validated =
        rumoca_test_msl::msl_tools::omc_simulation_reference::validate_merged_trace_report(
            &paths,
            report_bytes,
            reference_bytes,
            shard_witnesses,
        )
        .map_err(|error| format!("{error:#}"))?;
    checked_trace_comparison_payload(validated, Path::new("<merged-shard-trace-comparison>"))
        .map_err(|error| error.to_string())
}

#[cfg(test)]
pub(crate) fn fixture_trace_comparison_receipt() -> CurrentRunTraceComparison {
    use rumoca_sim::sim_trace_compare::{SimTrace, compare_model_traces};
    let temp = tempfile::tempdir().expect("receipt fixture dir");
    let results = temp.path().join("results");
    fs::create_dir_all(results.join("sim_traces/omc")).expect("OMC trace fixture dir");
    fs::create_dir_all(results.join("sim_traces/rumoca")).expect("Rumoca trace fixture dir");
    let trace = SimTrace {
        model_name: Some("M".to_string()),
        times: vec![0.0, 1.0],
        names: vec!["x".to_string()],
        data: vec![vec![Some(0.0), Some(1.0)]],
        variable_meta: None,
        certification_profile: None,
    };
    let trace_bytes = serde_json::to_vec(&trace).expect("serialize source trace fixture");
    fs::write(results.join("sim_traces/omc/source.json"), &trace_bytes).expect("write OMC trace");
    fs::write(results.join("sim_traces/rumoca/source.json"), &trace_bytes)
        .expect("write Rumoca trace");
    let mut metric = serde_json::to_value(
        compare_model_traces("M", &trace, &trace).expect("compare trace fixture"),
    )
    .expect("serialize trace metric");
    let metric = metric.as_object_mut().expect("metric object");
    for key in [
        "state_selection",
        "rumoca_sim_wall_seconds",
        "rumoca_sim_seconds",
        "rumoca_sim_build_seconds",
        "rumoca_sim_run_seconds",
        "omc_sim_system_seconds",
        "omc_total_system_seconds",
        "omc_wall_seconds",
    ] {
        metric.insert(key.to_string(), serde_json::Value::Null);
    }
    let report = serde_json::json!({
        "models_compared": 1,
        "models": { "M": metric },
        "missing_trace": {}, "skipped": {}, "trace_nonidentifiable": {}
    });
    let reference = serde_json::json!({ "models": { "M": {
        "status": "success", "error": null, "sim_system_seconds": null,
        "total_system_seconds": null, "omc_wall_seconds": null, "result_file": null,
        "trace_file": "sim_traces/omc/source.json", "trace_error": null,
        "rumoca_status": "sim_ok", "rumoca_ic_status": null, "rumoca_ic_error": null,
        "rumoca_ic_seconds": null, "rumoca_sim_seconds": null,
        "rumoca_sim_build_seconds": null, "rumoca_sim_run_seconds": null,
        "rumoca_sim_wall_seconds": null, "rumoca_trace_file": "sim_traces/rumoca/source.json",
        "rumoca_trace_error": null, "failed_attempts": 0
    }}});
    let paths = rumoca_test_msl::msl_tools::common::MslPaths::current().with_results_dir(&results);
    let validated = rumoca_test_msl::msl_tools::omc_simulation_reference::validate_trace_report_against_sources(
        &paths,
        &serde_json::to_vec(&report).expect("serialize report fixture"),
        &serde_json::to_vec(&reference).expect("serialize reference fixture"),
    )
    .expect("validate receipt fixture");
    checked_trace_comparison_payload(validated, Path::new("<test-trace-comparison>"))
        .expect("valid receipt fixture")
}

fn load_sim_parity_targets() -> io::Result<(PathBuf, Vec<String>)> {
    let sim_targets_path = msl_simulation_targets_path();
    let sim_targets = load_target_model_names(&sim_targets_path).map_err(|error| {
        io::Error::other(format!(
            "failed to load simulation targets '{}': {}",
            sim_targets_path.display(),
            error
        ))
    })?;
    Ok((sim_targets_path, sim_targets))
}

struct ParityStepContext {
    tools_exe: PathBuf,
    omc_version: String,
    workers: usize,
    omc_threads: usize,
}

fn run_simulation_parity_reference_command(
    context: &ParityStepContext,
    sim_targets_path: &Path,
    resume: bool,
) -> io::Result<()> {
    let sim_targets_arg = sim_targets_path.to_string_lossy().to_string();
    let mut args = vec![
        "omc-simulation-reference".to_string(),
        "--target-models-file".to_string(),
        sim_targets_arg,
        "--results-dir".to_string(),
        msl_results_dir().to_string_lossy().to_string(),
        "--use-experiment-stop-time".to_string(),
        "--model-timeout-seconds".to_string(),
        omc_sim_reference_timeout_secs().to_string(),
        "--workers".to_string(),
        context.workers.to_string(),
        "--omc-threads".to_string(),
        context.omc_threads.to_string(),
    ];
    // The canonical flow restricts the OMC baseline to models rumoca already
    // simulates, which keeps the gate fast. The long-budget diagnostic lanes opt
    // out via `all_omc_targets` because their whole point is to compare models
    // that are not yet `sim_ok`, and those need an OMC reference to compare to.
    if parity_config().all_omc_targets != Some(true) {
        args.push("--rumoca-sim-ok-only".to_string());
    }
    // The tool reuses cached OMC results by default (keyed on OMC + MSL source).
    // On a parity cache miss we want a fresh OMC run, so force it; on a cache hit
    // (`resume`) we let the default cache reuse stand.
    if !resume {
        args.push("--force".to_string());
    }
    run_msl_tool_command(&context.tools_exe, args)
}

fn ensure_simulation_parity_reference(
    summary: &MslSummary,
    force_refresh: bool,
    context: &ParityStepContext,
    sim_targets_path: &Path,
    sim_targets: &[String],
) -> io::Result<()> {
    let _sim_ref_watchdog = StageAbortWatchdog::new("parity_simulation_reference", 3600);
    let sim_policy = current_simulation_parity_cache_policy();
    let omc_simulation_reference = omc_simulation_reference_path();
    let sim_cache_key = simulation_parity_cache_key(
        sim_targets,
        &summary.msl_version,
        &context.omc_version,
        sim_policy,
    );
    let sim_cache_entry = parity_cache_entry_path("simulation", &sim_cache_key);

    let keyed_cache_matches = simulation_parity_cache_matches(
        &sim_cache_entry,
        sim_targets,
        &summary.msl_version,
        &context.omc_version,
        sim_policy,
    )?;
    if !force_refresh && keyed_cache_matches {
        materialize_simulation_parity_cache_entry(&sim_cache_entry, &omc_simulation_reference)?;
        println!(
            "MSL parity cache hit: reusing {} via keyed cache {} (refreshing Rumoca trace comparison via --resume)",
            omc_simulation_reference.display(),
            sim_cache_entry.display()
        );
        run_simulation_parity_reference_command(context, sim_targets_path, true)?;
        persist_simulation_parity_cache_entry(&omc_simulation_reference, &sim_cache_entry)?;
        return Ok(());
    }

    let canonical_cache_matches =
        simulation_parity_cache_matches(
            &omc_simulation_reference,
            sim_targets,
            &summary.msl_version,
            &context.omc_version,
            sim_policy,
        )? && simulation_parity_cache_has_required_metrics(&omc_simulation_reference)?;
    if force_refresh || !canonical_cache_matches {
        println!(
            "MSL parity cache miss/incomplete for simulation reference; regenerating {}",
            omc_simulation_reference.display()
        );
        run_simulation_parity_reference_command(context, sim_targets_path, false)?;
    } else {
        println!(
            "MSL parity cache hit: reusing {} (refreshing Rumoca trace comparison via --resume)",
            omc_simulation_reference.display()
        );
        run_simulation_parity_reference_command(context, sim_targets_path, true)?;
    }
    persist_simulation_parity_cache_entry(&omc_simulation_reference, &sim_cache_entry)?;
    Ok(())
}

/// Run the OMC reference + trace-comparison stage.
///
/// # Acceptance contract (SPEC 0008)
///
/// Returns [`MslParityStageOutcome::Ran`] only when the comparator command
/// completed and left a reference the gate can read. Every other path returns
/// [`MslParityStageOutcome::DidNotRun`] carrying the reason that stopped it —
/// there is no `Ok(())` that means "nothing happened, carry on". Rejected
/// (i.e. reported as `DidNotRun`, never swallowed): `omc` missing from PATH, a
/// missing `rumoca-msl-tools` binary, an unreadable simulation-target list, and
/// a comparator command that exits nonzero, and a run with zero simulations
/// attempted. The latter has nothing to compare and carries that typed reason.
/// Owner: this function; the consumer that turns the outcome into a verdict is
/// [`measure_msl_parity`].
pub(crate) fn ensure_required_msl_parity_references(summary: &MslSummary) -> MslParityStageOutcome {
    let output = match CurrentTraceComparisonOutput::claim() {
        Ok(output) => output,
        Err(error) => {
            return MslParityStageOutcome::DidNotRun(reference_stage_failure(error));
        }
    };
    ensure_required_msl_parity_references_with_output(summary, output)
}

fn ensure_required_msl_parity_references_with_output(
    summary: &MslSummary,
    output: CurrentTraceComparisonOutput,
) -> MslParityStageOutcome {
    if summary.sim_attempted == 0 {
        return MslParityStageOutcome::DidNotRun(MslParityUnmeasuredReason::NoSimulationsAttempted);
    }
    match run_msl_parity_reference_stage(summary, output) {
        Ok(receipt) => MslParityStageOutcome::Ran(receipt),
        Err(error) => MslParityStageOutcome::DidNotRun(reference_stage_failure(error)),
    }
}

/// Map a stage error onto the boundary that produced it, so the summary names
/// the missing tool rather than the downstream missing file.
fn reference_stage_failure(error: io::Error) -> MslParityUnmeasuredReason {
    let detail = error.to_string();
    if detail.starts_with(OMC_UNAVAILABLE_PREFIX) {
        return MslParityUnmeasuredReason::OmcUnavailable {
            detail: detail
                .trim_start_matches(OMC_UNAVAILABLE_PREFIX)
                .trim()
                .to_string(),
        };
    }
    MslParityUnmeasuredReason::ComparatorStageFailed { detail }
}

const OMC_UNAVAILABLE_PREFIX: &str = "omc unavailable:";

fn run_msl_parity_reference_stage(
    summary: &MslSummary,
    trace_comparison_output: CurrentTraceComparisonOutput,
) -> io::Result<CurrentRunTraceComparison> {
    let stage_start = Instant::now();
    let force_refresh = force_omc_parity_refresh_enabled();
    // A successful subprocess is not proof that it rewrote its output. Remove
    // the prior name before launching it, so the receipt below can only name a
    // payload produced by this invocation.
    let (sim_targets_path, sim_targets) = load_sim_parity_targets()?;
    let omc_version = current_omc_version()
        .map_err(|error| io::Error::other(format!("{OMC_UNAVAILABLE_PREFIX} {error}")))?;
    let context = ParityStepContext {
        tools_exe: resolve_msl_tools_exe()?,
        omc_version,
        workers: omc_parity_workers(),
        omc_threads: omc_parity_threads(),
    };
    println!(
        "MSL parity targets: simulation={} (workers={})",
        sim_targets.len(),
        context.workers
    );

    // The OMC reference comes solely from the persistent-zmq simulation pass,
    // which compiles each model as part of simulating it. (The removed non-zmq
    // `omc-reference` compile pass reloaded the full MSL library per batch and
    // timed out on CI without adding data the sim pass lacks.)
    let sim_ref_start = Instant::now();
    ensure_simulation_parity_reference(
        summary,
        force_refresh,
        &context,
        &sim_targets_path,
        &sim_targets,
    )?;
    println!(
        "MSL parity simulation reference step: {:.2}s",
        sim_ref_start.elapsed().as_secs_f64()
    );

    let receipt = trace_comparison_output.issue()?;
    println!(
        "MSL parity total step time: {:.2}s",
        stage_start.elapsed().as_secs_f64()
    );
    Ok(receipt)
}

pub(crate) fn current_omc_parity_workers() -> usize {
    omc_parity_workers()
}

pub(crate) fn current_omc_parity_threads() -> usize {
    omc_parity_threads()
}

pub(super) fn simulation_parity_cache_has_required_metrics(path: &Path) -> io::Result<bool> {
    if !path.is_file() {
        return Ok(false);
    }
    let parity = load_msl_parity_gate_input(path)?;
    let Some(runtime_stats) = parity.runtime_ratio_stats else {
        return Ok(false);
    };
    let Some(trace_stats) = parity.trace_accuracy_stats else {
        return Ok(false);
    };

    Ok(runtime_stats.system_ratio_both_success.sample_count > 0
        && runtime_stats.wall_ratio_both_success.sample_count > 0
        && trace_stats.models_compared > 0
        && parity.omc_assertion_failure_models == 0
        && trace_stats
            .state_selection
            .as_ref()
            .is_some_and(|stats| stats.models_compared > 0))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Reproduce the "`omc` is not on PATH" error exactly as
    /// [`current_omc_version`] would surface it: spawning a binary that does not
    /// exist yields the same `io::Error` kind and message shape, without
    /// mutating the process environment.
    fn omc_missing_error() -> io::Error {
        std::process::Command::new("rumoca-definitely-not-omc")
            .arg("--version")
            .output()
            .expect_err("spawning a nonexistent binary must fail")
    }

    #[test]
    fn a_missing_omc_binary_is_reported_as_omc_unavailable_not_a_generic_failure() {
        let wrapped = io::Error::other(format!("{OMC_UNAVAILABLE_PREFIX} {}", omc_missing_error()));
        let reason = reference_stage_failure(wrapped);
        match &reason {
            MslParityUnmeasuredReason::OmcUnavailable { detail } => {
                assert!(
                    !detail.is_empty(),
                    "the OS error must survive into the summary"
                );
                assert!(
                    !detail.contains(OMC_UNAVAILABLE_PREFIX),
                    "the marker prefix must be stripped, got: {detail}"
                );
            }
            other => panic!("a missing omc must name the missing tool, got {other:?}"),
        }
        assert!(
            reason.detail().contains("omc is not available on PATH"),
            "operator-facing text must name PATH, got: {}",
            reason.detail()
        );
    }

    #[test]
    fn any_other_stage_error_is_reported_as_a_comparator_failure_never_swallowed() {
        let reason = reference_stage_failure(io::Error::other(
            "command 'rumoca-msl-tools' failed (status=exit status: 1)",
        ));
        match &reason {
            MslParityUnmeasuredReason::ComparatorStageFailed { detail } => {
                assert!(detail.contains("rumoca-msl-tools"), "got: {detail}");
            }
            other => panic!("a comparator command failure must be named, got {other:?}"),
        }
    }

    #[test]
    fn a_run_with_no_simulations_reports_that_reason_rather_than_running_omc() {
        let dir = tempfile::tempdir().expect("temporary results directory");
        let output =
            CurrentTraceComparisonOutput::claim_at(dir.path().join(TRACE_COMPARISON_FILE_REL))
                .expect("claim test output slot");
        let mut summary = super::super::super::empty_summary(1, 0);
        summary.total_models = 1;
        summary.sim_attempted = 0;
        assert_eq!(
            ensure_required_msl_parity_references_with_output(&summary, output),
            MslParityStageOutcome::DidNotRun(MslParityUnmeasuredReason::NoSimulationsAttempted)
        );
    }

    #[test]
    fn a_stale_trace_file_cannot_mint_a_current_run_receipt() {
        let dir = tempfile::tempdir().expect("temporary results directory");
        let path = dir.path().join(TRACE_COMPARISON_FILE_REL);
        fs::write(
            &path,
            serde_json::to_vec(&serde_json::json!({
                "models_compared": 1,
                "models": { "Stale": {} }
            }))
            .expect("serialize stale fixture"),
        )
        .expect("write stale fixture");

        let output = CurrentTraceComparisonOutput::claim_at(path.clone())
            .expect("claim current-run output slot");
        let error = output
            .issue()
            .expect_err("a successful stage that wrote nothing cannot recover stale evidence");

        assert!(
            error
                .to_string()
                .contains("returned success without producing"),
            "got: {error}"
        );
        assert!(!path.exists(), "the stale artifact stays invalidated");
    }

    #[test]
    fn failed_stage_removes_partial_current_output() {
        let dir = tempfile::tempdir().expect("temporary results directory");
        let path = dir.path().join(TRACE_COMPARISON_FILE_REL);
        let output = CurrentTraceComparisonOutput::claim_at(path.clone())
            .expect("claim current-run output slot");
        fs::write(&path, b"partial comparator output").expect("write partial output");

        drop(output);

        assert!(
            !path.exists(),
            "unissued partial output cannot survive a failed comparator stage"
        );
    }

    #[test]
    fn receipt_issuer_rejects_vacuous_or_inconsistent_payloads() {
        for payload in [
            serde_json::json!({ "models_compared": 0, "models": {} }),
            serde_json::json!({ "models_compared": 2, "models": { "OnlyOne": {} } }),
        ] {
            assert!(
                ensure_trace_comparison_nonvacuous(&payload, Path::new("fixture.json")).is_err(),
                "a receipt cannot represent vacuous or truncated evidence"
            );
        }
    }
}
