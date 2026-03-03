use super::*;

// =============================================================================
// Summary aggregation and timing helpers
// =============================================================================

pub(super) fn summarize_msl_results(results: &[MslModelResult]) -> ResultCounters {
    let mut counters = ResultCounters::default();
    for result in results {
        match result.phase_reached.as_str() {
            "Success" => process_success_result(result, &mut counters),
            "NeedsInner" => process_phase_failure(result, "NeedsInner", &mut counters),
            "Instantiate" => process_phase_failure(result, "Instantiate", &mut counters),
            "NonSim" => process_non_sim_result(result, &mut counters),
            "Typecheck" => process_phase_failure(result, "Typecheck", &mut counters),
            "Flatten" => process_flatten_error(result, &mut counters),
            "ToDae" => process_phase_failure(result, "ToDae", &mut counters),
            _ => {}
        }
        // Count simulation results
        if let Some(ref status) = result.sim_status {
            counters.sim_attempted += 1;
            counters.total_sim_seconds += result.sim_seconds.unwrap_or(0.0);
            counters.total_sim_wall_seconds += result.sim_wall_seconds.unwrap_or(0.0);
            match status.as_str() {
                "sim_ok" => counters.sim_ok += 1,
                "sim_nan" => counters.sim_nan += 1,
                "sim_solver_fail" => counters.sim_solver_fail += 1,
                "sim_timeout" => counters.sim_timeout += 1,
                "sim_balance_fail" => counters.sim_balance_fail += 1,
                _ => {}
            }
        }
    }
    counters
}

pub(super) fn finalize_msl_summary_from_results(
    model_results: Vec<MslModelResult>,
    sim_target_models: Vec<String>,
    inputs: MslSummaryInputs,
    mut timings: MslPhaseTimings,
    core_start: Instant,
) -> MslSummary {
    let summarize_start = Instant::now();
    let counters = summarize_msl_results(&model_results);
    timings.summarize_seconds = summarize_start.elapsed().as_secs_f64();
    timings.core_pipeline_seconds = core_start.elapsed().as_secs_f64();
    println!(
        "Aggregated summary stats in {:.2}s",
        timings.summarize_seconds
    );
    println!(
        "Core MSL pipeline (excluding JSON write) finished in {:.2}s",
        timings.core_pipeline_seconds
    );

    build_summary_from_counters(inputs, timings, counters, model_results, sim_target_models)
}

fn build_summary_from_counters(
    inputs: MslSummaryInputs,
    timings: MslPhaseTimings,
    counters: ResultCounters,
    model_results: Vec<MslModelResult>,
    sim_target_models: Vec<String>,
) -> MslSummary {
    MslSummary {
        git_commit: current_git_commit(),
        msl_version: MSL_VERSION.to_string(),
        total_mo_files: inputs.total_mo_files,
        parse_errors: inputs.parse_errors,
        resolve_errors: 0,
        typecheck_errors: 0,
        total_models: inputs.total_models,
        needs_inner: counters.needs_inner,
        instantiate_failed: counters.instantiate_failed,
        typecheck_failed: counters.typecheck_failed,
        flatten_failed: counters.flatten_failed,
        todae_failed: counters.todae_failed,
        non_sim_models: counters.non_sim_models,
        compiled_models: counters.compiled_models,
        balanced_models: counters.balanced_models,
        unbalanced_models: counters.unbalanced_models,
        initial_balanced_models: counters.initial_balanced_models,
        initial_unbalanced_models: counters.initial_unbalanced_models,
        partial_models: counters.partial_models,
        class_type_counts: inputs.class_type_counts,
        failures_by_phase: counters.failures_by_phase,
        unbalanced_list: counters.unbalanced_list,
        initial_unbalanced_list: counters.initial_unbalanced_list,
        non_sim_list: counters.non_sim_list,
        error_categories: counters.error_categories,
        undefined_vars: counters.undefined_vars,
        balance_distribution: counters.balance_distribution,
        model_results,
        timings,
        sim_ok: counters.sim_ok,
        sim_nan: counters.sim_nan,
        sim_solver_fail: counters.sim_solver_fail,
        sim_timeout: counters.sim_timeout,
        sim_balance_fail: counters.sim_balance_fail,
        sim_attempted: counters.sim_attempted,
        total_sim_seconds: counters.total_sim_seconds,
        total_sim_wall_seconds: counters.total_sim_wall_seconds,
        sim_target_models,
    }
}

pub(super) fn update_phase_timing_totals(timings: &mut MslPhaseTimings) {
    let compile_phase_timings = compile_phase_timing_stats();
    let flatten_phase_timings = flatten_phase_timing_stats();

    timings.compile_instantiate_seconds = compile_phase_timings.instantiate.total_seconds();
    timings.compile_typecheck_seconds = compile_phase_timings.typecheck.total_seconds();
    timings.compile_flatten_seconds = compile_phase_timings.flatten.total_seconds();
    timings.compile_todae_seconds = compile_phase_timings.todae.total_seconds();
    timings.compile_instantiate_calls = compile_phase_timings.instantiate.calls;
    timings.compile_typecheck_calls = compile_phase_timings.typecheck.calls;
    timings.compile_flatten_calls = compile_phase_timings.flatten.calls;
    timings.compile_todae_calls = compile_phase_timings.todae.calls;
    timings.flatten_connections_seconds = flatten_phase_timings.connections.total_seconds();
    timings.flatten_connections_calls = flatten_phase_timings.connections.calls;
    timings.flatten_eval_fallback_seconds = flatten_phase_timings.eval_fallback.total_seconds();
    timings.flatten_eval_fallback_calls = flatten_phase_timings.eval_fallback.calls;
}

pub(super) fn print_compile_timing_summary(compiled_count: usize, timings: &MslPhaseTimings) {
    println!(
        "Compiled {} models in {:.2}s ({:.0} models/sec, parse+session+compile)",
        compiled_count,
        timings.frontend_compile_seconds,
        compiled_count as f64 / timings.frontend_compile_seconds
    );
    println!(
        "  Breakdown: parse {:.2}s, session {:.2}s, compile {:.2}s",
        timings.parse_seconds, timings.session_build_seconds, timings.compile_seconds
    );
    println!(
        "  Compile phase totals: instantiate {:.2}s ({} calls), typecheck {:.2}s ({} calls), flatten {:.2}s ({} calls), todae {:.2}s ({} calls)",
        timings.compile_instantiate_seconds,
        timings.compile_instantiate_calls,
        timings.compile_typecheck_seconds,
        timings.compile_typecheck_calls,
        timings.compile_flatten_seconds,
        timings.compile_flatten_calls,
        timings.compile_todae_seconds,
        timings.compile_todae_calls
    );
    println!(
        "  Flatten subpasses: connections {:.2}s ({} calls), eval fallback {:.2}s ({} calls)",
        timings.flatten_connections_seconds,
        timings.flatten_connections_calls,
        timings.flatten_eval_fallback_seconds,
        timings.flatten_eval_fallback_calls
    );
}

pub(super) fn prepare_sim_trace_dirs(run_simulation: bool) {
    if !run_simulation {
        return;
    }
    let rumoca_trace_dir = get_msl_cache_dir()
        .join("results")
        .join("sim_traces")
        .join("rumoca");
    if rumoca_trace_dir.exists() {
        let _ = fs::remove_dir_all(&rumoca_trace_dir);
    }
    let _ = fs::create_dir_all(&rumoca_trace_dir);
}
