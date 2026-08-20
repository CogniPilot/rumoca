//! The OMC reference + trace-comparison stage of the MSL parity gate.
//!
//! Split out of `balance_pipeline_quality_gate.rs` so the stage that PRODUCES a
//! parity reading lives apart from the gate that JUDGES one. Every exit from
//! this module is typed: see `ensure_required_msl_parity_references` for the
//! acceptance contract.

use super::*;

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
/// a comparator command that exits nonzero. Accepted: a run with zero
/// simulations attempted, which has nothing to compare and says so.
/// Owner: this function; the consumer that turns the outcome into a verdict is
/// [`measure_msl_parity`].
pub(crate) fn ensure_required_msl_parity_references(summary: &MslSummary) -> MslParityStageOutcome {
    if summary.sim_attempted == 0 {
        return MslParityStageOutcome::DidNotRun(MslParityUnmeasuredReason::NoSimulationsAttempted);
    }
    match run_msl_parity_reference_stage(summary) {
        Ok(()) => MslParityStageOutcome::Ran,
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

fn run_msl_parity_reference_stage(summary: &MslSummary) -> io::Result<()> {
    let stage_start = Instant::now();
    let force_refresh = force_omc_parity_refresh_enabled();

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

    let _ = load_current_msl_parity_gate_input_required(sim_targets.len())?;
    println!(
        "MSL parity total step time: {:.2}s",
        stage_start.elapsed().as_secs_f64()
    );
    Ok(())
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
        let mut summary = super::super::super::empty_summary(1, 0);
        summary.total_models = 1;
        summary.sim_attempted = 0;
        assert_eq!(
            ensure_required_msl_parity_references(&summary),
            MslParityStageOutcome::DidNotRun(MslParityUnmeasuredReason::NoSimulationsAttempted)
        );
    }
}
