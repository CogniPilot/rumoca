use super::*;

// =============================================================================
// Human-readable simulation/timing/failure reporting
// =============================================================================

pub(super) fn print_simulation_results(summary: &MslSummary) {
    println!("Simulation Results:");
    println!("  - Attempted: {}", summary.sim_attempted);
    println!("  - sim_ok: {}", summary.sim_ok);
    println!("  - sim_nan: {}", summary.sim_nan);
    println!("  - sim_solver_fail: {}", summary.sim_solver_fail);
    println!("  - sim_timeout: {}", summary.sim_timeout);
    println!("  - sim_balance_fail: {}", summary.sim_balance_fail);
    println!(
        "  - Total sim solver time: {:.2}s (sum of per-model worker-reported solver runtime)",
        summary.total_sim_seconds
    );
    println!(
        "  - Total sim wall/system time: {:.2}s (sum of per-model process wall time)",
        summary.total_sim_wall_seconds
    );
    let trace_count = summary
        .model_results
        .iter()
        .filter(|result| result.sim_trace_file.is_some())
        .count();
    let trace_write_fail_count = summary
        .model_results
        .iter()
        .filter(|result| result.sim_trace_error.is_some())
        .count();
    println!("  - Trace files written: {}", trace_count);
    println!("  - Trace write errors: {}", trace_write_fail_count);
    if summary.sim_attempted > 0 {
        let sim_rate = (summary.sim_ok as f64 / summary.sim_attempted as f64) * 100.0;
        println!(
            "  - Simulation success rate: {:.1}% ({}/{})",
            sim_rate, summary.sim_ok, summary.sim_attempted
        );
    }
    // Print per-model simulation details
    for result in &summary.model_results {
        if let Some(ref status) = result.sim_status {
            let error_str = result
                .sim_error
                .as_deref()
                .map(|e| format!(" — {}", e))
                .unwrap_or_default();
            println!("  [{}] {}{error_str}", status, result.model_name,);
        }
    }
    println!();
}

fn hottest_compile_model(summary: &MslSummary) -> Option<(&str, f64)> {
    summary
        .model_results
        .iter()
        .filter_map(|result| {
            result
                .compile_seconds
                .map(|seconds| (result.model_name.as_str(), seconds))
        })
        .max_by(|(_, lhs), (_, rhs)| lhs.total_cmp(rhs))
}

fn hottest_sim_model(summary: &MslSummary) -> Option<(&str, f64)> {
    summary
        .model_results
        .iter()
        .filter_map(|result| {
            result
                .sim_wall_seconds
                .map(|seconds| (result.model_name.as_str(), seconds))
        })
        .max_by(|(_, lhs), (_, rhs)| lhs.total_cmp(rhs))
}

fn print_profiler_follow_ups(summary: &MslSummary) {
    println!("Profiler Follow-Ups:");
    if let Some((model_name, seconds)) = hottest_compile_model(summary) {
        println!("  - Hottest compile model: {model_name} ({seconds:.2}s)");
        println!(
            "    cargo run --bin rum -- repo msl flamegraph --model {model_name} --mode compile"
        );
    }
    if let Some((model_name, seconds)) = hottest_sim_model(summary) {
        println!("  - Hottest sim model: {model_name} ({seconds:.2}s)");
        println!(
            "    cargo run --bin rum -- repo msl flamegraph --model {model_name} --mode simulate"
        );
    }
    println!();
}

pub(super) fn print_final_stats(summary: &MslSummary) {
    let write_start = Instant::now();
    write_msl_results(summary).expect("Failed to write results");
    let json_write_seconds = write_start.elapsed().as_secs_f64();
    println!("  - JSON results write: {:.2}s", json_write_seconds);
    println!(
        "  - Core + JSON write subtotal: {:.2}s",
        summary.timings.core_pipeline_seconds + json_write_seconds
    );
    assert_valid_msl_summary(summary);
    if summary.sim_attempted == 0 {
        enforce_msl_quality_gate(summary).expect("Failed to run MSL quality gate");
    } else {
        let parity_start = Instant::now();
        let _parity_watchdog =
            StageAbortWatchdog::new("parity_stage", "RUMOCA_MSL_STAGE_TIMEOUT_PARITY_SECS", 2400);
        println!("MSL parity stage: ensuring OMC references + trace comparison...");
        ensure_required_msl_parity_references(summary)
            .expect("Failed to ensure required OMC parity references");
        println!(
            "MSL parity stage: completed in {:.2}s",
            parity_start.elapsed().as_secs_f64()
        );
        let quality_snapshot_start = Instant::now();
        let _snapshot_watchdog = StageAbortWatchdog::new(
            "quality_snapshot_write",
            "RUMOCA_MSL_STAGE_TIMEOUT_SNAPSHOT_WRITE_SECS",
            300,
        );
        println!("MSL parity stage: writing current quality snapshot...");
        write_current_msl_quality_snapshot(summary)
            .expect("Failed to write current MSL quality snapshot");
        println!(
            "MSL parity stage: quality snapshot written in {:.2}s",
            quality_snapshot_start.elapsed().as_secs_f64()
        );
        if should_skip_msl_quality_gate() {
            println!(
                "MSL quality gate: baseline delta checks skipped for non-baseline run (focused subset or non-default RUMOCA_MSL_SIM_SET)."
            );
            enforce_msl_quality_gate(summary).expect("Failed to run MSL quality gate");
        } else {
            let quality_gate_start = Instant::now();
            let _quality_gate_watchdog = StageAbortWatchdog::new(
                "quality_gate_eval",
                "RUMOCA_MSL_STAGE_TIMEOUT_QUALITY_GATE_SECS",
                300,
            );
            println!("MSL quality gate: evaluating baseline deltas...");
            enforce_msl_quality_gate(summary).expect("Failed to run MSL quality gate");
            println!(
                "MSL quality gate: completed in {:.2}s",
                quality_gate_start.elapsed().as_secs_f64()
            );
        }
    }

    let attempted_standalone = summary.compiled_models
        + summary.resolve_failed
        + summary.instantiate_failed
        + summary.typecheck_failed
        + summary.flatten_failed
        + summary.todae_failed;
    let compile_rate = if attempted_standalone > 0 {
        (summary.compiled_models as f64 / attempted_standalone as f64) * 100.0
    } else {
        0.0
    };

    println!(
        "\nSimulatable compilation rate: {:.1}% ({} compiled / {} simulatable models)",
        compile_rate, summary.compiled_models, attempted_standalone
    );
    println!(
        "Non-simulatable non-partial models (excluded from simulatable denominator): {}",
        summary.non_sim_models
    );
}

pub(super) fn print_timing_breakdown(summary: &MslSummary) {
    println!("Performance Snapshot:");
    println!(
        "  - Compile chunking: {} chunk(s) of up to {} model(s)",
        summary.timings.compile_chunk_count, summary.timings.compile_batch_size
    );
    println!("  - Worker threads: {}", summary.timings.worker_threads);
    println!(
        "  - Compile-scope throughput: {:.2} models/s",
        summary.total_models as f64 / summary.timings.frontend_compile_seconds.max(f64::EPSILON)
    );
    if summary.timings.render_and_write_seconds > 0.0 && summary.sim_attempted > 0 {
        println!(
            "  - Sim/render throughput: {:.2} models/s",
            summary.sim_attempted as f64 / summary.timings.render_and_write_seconds
        );
    }
    println!(
        "  - Core pipeline subtotal: {:.2}s",
        summary.timings.core_pipeline_seconds
    );
    println!();
    print_profiler_follow_ups(summary);
    println!("Timing Breakdown:");
    println!(
        "  - Pipeline compile (parse+session+compile): {:.2}s",
        summary.timings.frontend_compile_seconds
    );
    println!("  - Parse: {:.2}s", summary.timings.parse_seconds);
    println!(
        "  - Session build: {:.2}s",
        summary.timings.session_build_seconds
    );
    println!("  - Compile only: {:.2}s", summary.timings.compile_seconds);
    println!(
        "  - Compile phase totals: instantiate {:.2}s ({} calls), typecheck {:.2}s ({} calls), flatten {:.2}s ({} calls), todae {:.2}s ({} calls)",
        summary.timings.compile_instantiate_seconds,
        summary.timings.compile_instantiate_calls,
        summary.timings.compile_typecheck_seconds,
        summary.timings.compile_typecheck_calls,
        summary.timings.compile_flatten_seconds,
        summary.timings.compile_flatten_calls,
        summary.timings.compile_todae_seconds,
        summary.timings.compile_todae_calls
    );
    println!(
        "  - Flatten subpasses: connections {:.2}s ({} calls), eval fallback {:.2}s ({} calls)",
        summary.timings.flatten_connections_seconds,
        summary.timings.flatten_connections_calls,
        summary.timings.flatten_eval_fallback_seconds,
        summary.timings.flatten_eval_fallback_calls
    );
    println!(
        "  - File generation (render + write): {:.2}s",
        summary.timings.render_and_write_seconds
    );
    println!(
        "  - Summary aggregation: {:.2}s",
        summary.timings.summarize_seconds
    );
    println!(
        "  - Core pipeline subtotal (before JSON write): {:.2}s",
        summary.timings.core_pipeline_seconds
    );
}

/// Print detailed failure and error information from the balance summary.
pub(super) fn print_failure_details(summary: &MslSummary) {
    // Print first few failures by phase (skip NeedsInner since those aren't failures)
    for (phase, failures) in &summary.failures_by_phase {
        if phase == "NeedsInner" || phase == "NonSim" {
            continue;
        }
        println!("\nFirst 5 {} failures:", phase);
        for model in failures.iter().take(5) {
            println!("  - {}", model);
        }
        if failures.len() > 5 {
            println!("  ... and {} more", failures.len() - 5);
        }
    }

    // Print first few unbalanced models
    if !summary.unbalanced_list.is_empty() {
        println!("\nFirst 5 unbalanced models:");
        for model in summary.unbalanced_list.iter().take(5) {
            println!("  - {}", model);
        }
        if summary.unbalanced_list.len() > 5 {
            println!("  ... and {} more", summary.unbalanced_list.len() - 5);
        }
    }

    if !summary.initial_unbalanced_list.is_empty() {
        println!("\nFirst 5 models with initial-balance deficits:");
        for model in summary.initial_unbalanced_list.iter().take(5) {
            println!("  - {}", model);
        }
        if summary.initial_unbalanced_list.len() > 5 {
            println!(
                "  ... and {} more",
                summary.initial_unbalanced_list.len() - 5
            );
        }
    }

    // Print flatten error categories (if any)
    if !summary.error_categories.is_empty() {
        println!("\n=== Flatten Error Categories ===");
        let mut sorted: Vec<_> = summary.error_categories.iter().collect();
        sorted.sort_by_key(|(_, errors)| std::cmp::Reverse(errors.len()));

        for (category, errors) in &sorted {
            println!("\n--- {} ({} errors) ---", category, errors.len());
            for (model, error) in errors.iter().take(5) {
                let short_error = truncate_error(error, 100);
                println!("  {}: {}", model, short_error);
            }
            if errors.len() > 5 {
                println!("  ... and {} more", errors.len() - 5);
            }
        }

        // Summary by category
        println!("\n=== Error Summary by Category ===");
        for (category, errors) in &sorted {
            let pct = errors.len() as f64 / summary.flatten_failed as f64 * 100.0;
            println!("{:25} {:>5} ({:>5.1}%)", category, errors.len(), pct);
        }
    }

    // Print most common undefined variables (if any)
    if !summary.undefined_vars.is_empty() {
        println!("\n=== Most Common Undefined Variables ===");
        let mut sorted_vars: Vec<_> = summary.undefined_vars.iter().collect();
        sorted_vars.sort_by(|a, b| b.1.cmp(a.1));
        for (var, count) in sorted_vars.iter().take(10) {
            println!("  {} ({}x)", var, count);
        }
    }
}
