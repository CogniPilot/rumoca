use super::*;

pub(super) fn print_compile_and_sim_gate_pass(
    gate_input: MslQualityGateInput<'_>,
    baseline: &MslQualityBaseline,
) {
    let denominator = gate_input.simulatable_attempted;
    println!(
        "MSL stage gate: PASS parse={}/{} ({:.2}%, baseline={}), flatten={}/{} ({:.2}%, baseline={}), dae={}/{} ({:.2}%, baseline={}), ir_solve={}/{} ({:.2}%, baseline={}).",
        gate_input.parse_models,
        denominator,
        stage_percent(gate_input.parse_models, denominator),
        baseline.parse_models,
        gate_input.flatten_models,
        denominator,
        stage_percent(gate_input.flatten_models, denominator),
        baseline.flatten_models,
        gate_input.dae_models,
        denominator,
        stage_percent(gate_input.dae_models, denominator),
        baseline.dae_models,
        gate_input.solve_models,
        denominator,
        stage_percent(gate_input.solve_models, denominator),
        baseline.solve_models
    );
    let balance_rate =
        balance_success_rate(gate_input.balanced_models, gate_input.balance_denominator)
            .unwrap_or(0.0)
            * 100.0;
    let baseline_balance_rate =
        balance_success_rate(baseline.balanced_models, baseline.balance_denominator).unwrap_or(0.0)
            * 100.0;
    let initial_balance_rate = balance_success_rate(
        gate_input.initial_balanced_models,
        gate_input.balance_denominator,
    )
    .unwrap_or(0.0)
        * 100.0;
    let baseline_initial_balance_rate = balance_success_rate(
        baseline.initial_balanced_models,
        baseline.balance_denominator,
    )
    .unwrap_or(0.0)
        * 100.0;
    println!(
        "MSL balance gate: PASS balance={:.2}% (baseline={:.2}%), initial_balance={:.2}% (baseline={:.2}%).",
        balance_rate, baseline_balance_rate, initial_balance_rate, baseline_initial_balance_rate
    );
    if gate_input.sim_attempted > 0 {
        let current_ic_rate = sim_success_rate(gate_input.ic_ok, gate_input.sim_target_models)
            .expect("sim_target_models > 0 implies Some rate");
        let current_rate = sim_success_rate(gate_input.sim_ok, gate_input.sim_target_models)
            .expect("sim_target_models > 0 implies Some rate");
        println!(
            "MSL simulation gate: PASS ic={:.2}% ({}/{}; baseline={}/{}), sim={:.2}% ({}/{}; baseline={}/{}, commit={}).",
            current_ic_rate * 100.0,
            gate_input.ic_ok,
            gate_input.sim_target_models,
            baseline.ic_ok,
            baseline.sim_target_models,
            current_rate * 100.0,
            gate_input.sim_ok,
            gate_input.sim_target_models,
            baseline.sim_ok,
            baseline.sim_target_models,
            baseline.git_commit
        );
    } else {
        println!("MSL simulation gate: skipped (no simulations attempted in this run).");
    }
}

pub(super) fn print_trace_gate_status(
    baseline: &MslQualityBaseline,
    parity_input: Option<&MslParityGateInput>,
) {
    if let (Some(current_trace), Some(baseline_trace)) = (
        parity_input.and_then(|parity| parity.trace_accuracy_stats.as_ref()),
        baseline.trace_accuracy_stats.as_ref(),
    ) {
        let current_pct =
            trace_model_bucket_percentages(current_trace).unwrap_or(TraceBucketPercentages {
                high: 0.0,
                near: 0.0,
                deviation: 0.0,
            });
        let baseline_pct =
            trace_model_bucket_percentages(baseline_trace).unwrap_or(TraceBucketPercentages {
                high: 0.0,
                near: 0.0,
                deviation: 0.0,
            });
        let current_any =
            trace_models_with_any_channel_deviation_percent(current_trace).unwrap_or(0.0);
        let baseline_any =
            trace_models_with_any_channel_deviation_percent(baseline_trace).unwrap_or(0.0);
        let current_acceptable = trace_acceptable_agreement_models(current_trace);
        let baseline_acceptable = trace_acceptable_agreement_models(baseline_trace);
        let current_no_severe = trace_no_severe_models(current_trace).unwrap_or(0);
        let baseline_no_severe = trace_no_severe_models(baseline_trace).unwrap_or(0);
        println!("MSL trace gate: PASS with baseline:");
        println!(
            "  trace={:.2}% ({}/{}; baseline={}/{}), no_severe={:.2}% ({}/{}; baseline={}/{})",
            stage_percent(current_acceptable, baseline.sim_target_models),
            current_acceptable,
            baseline.sim_target_models,
            baseline_acceptable,
            baseline.sim_target_models,
            stage_percent(current_no_severe, baseline.sim_target_models),
            current_no_severe,
            baseline.sim_target_models,
            baseline_no_severe,
            baseline.sim_target_models
        );
        println!(
            "  compared-bucket high={:.2}% (baseline={:.2}%), near={:.2}% (baseline={:.2}%), deviation={:.2}% (baseline={:.2}%)",
            current_pct.high,
            baseline_pct.high,
            current_pct.near,
            baseline_pct.near,
            current_pct.deviation,
            baseline_pct.deviation,
        );
        println!(
            "  models_with_any_bad_channel={:.2}% (baseline={:.2}%), models_compared={} (baseline={})",
            current_any,
            baseline_any,
            current_trace.models_compared,
            baseline_trace.models_compared
        );
        if let (Some(current_bad), Some(baseline_bad)) = (
            trace_bad_channels_total(current_trace),
            trace_bad_channels_total(baseline_trace),
        ) {
            let current_severe = trace_severe_channels_total(current_trace).unwrap_or(0);
            let baseline_severe = trace_severe_channels_total(baseline_trace).unwrap_or(0);
            let current_mass = trace_violation_mass_total(current_trace).unwrap_or(0.0);
            let baseline_mass = trace_violation_mass_total(baseline_trace).unwrap_or(0.0);
            println!(
                "  bad_channels={} (baseline={}), severe_channels={} (baseline={}), violation_mass_total={:.6e} (baseline={:.6e})",
                current_bad,
                baseline_bad,
                current_severe,
                baseline_severe,
                current_mass,
                baseline_mass
            );
        }
        return;
    }
    if baseline.trace_accuracy_stats.is_some() {
        println!(
            "MSL trace gate: skipped (missing {}). Run `cargo run -p rumoca-test-msl --bin rumoca-msl-tools -- omc-simulation-reference ...` to enforce trace baseline.",
            omc_simulation_reference_path().display()
        );
    }
}

fn fmt_opt_usize(value: Option<usize>) -> String {
    value
        .map(|v| v.to_string())
        .unwrap_or_else(|| "n/a".to_string())
}

pub(super) fn print_runtime_ratio_status(
    baseline: &MslQualityBaseline,
    parity_input: Option<&MslParityGateInput>,
) {
    let Some(current_runtime) = parity_input.and_then(|parity| parity.runtime_ratio_stats.as_ref())
    else {
        return;
    };

    let current_workers = parity_input
        .and_then(|parity| parity.runtime_context.as_ref())
        .and_then(|context| context.workers_used);
    let current_omc_threads = parity_input
        .and_then(|parity| parity.runtime_context.as_ref())
        .and_then(|context| context.omc_threads);

    if let Some(baseline_runtime) = baseline.runtime_ratio_stats.as_ref() {
        println!(
            "MSL speed gate: PASS system_median={:.3e} (baseline={:.3e}), wall_median={:.3e} (baseline={:.3e}), workers={}, omc_threads={}.",
            current_runtime.system_ratio_both_success.median,
            baseline_runtime.system_ratio_both_success.median,
            current_runtime.wall_ratio_both_success.median,
            baseline_runtime.wall_ratio_both_success.median,
            fmt_opt_usize(current_workers),
            fmt_opt_usize(current_omc_threads)
        );
        return;
    }

    println!(
        "MSL speed metrics: observed system_median={:.3e}, wall_median={:.3e}, workers={}, omc_threads={}; no runtime baseline is committed yet.",
        current_runtime.system_ratio_both_success.median,
        current_runtime.wall_ratio_both_success.median,
        fmt_opt_usize(current_workers),
        fmt_opt_usize(current_omc_threads)
    );
}
