use std::collections::{HashMap, HashSet};

use rumoca_phase_structural::eliminate;

use crate::reconstruct;
use crate::sim_types::{OutputBuffers, SimError, SimOptions, SimResult};
use crate::simulation::diagnostics::{
    dump_parameter_vector, sim_introspect_enabled, sim_trace_enabled,
};
use crate::timeline;
use crate::{
    NoStateSampleContext, count_states, default_params_with_budget, finalize_algebraic_outputs,
    initialize_state_vector, project_algebraics_with_fixed_states_at_time,
    runtime_projection_required, seed_runtime_direct_assignments,
};

struct AlgebraicResultSetup {
    times: Vec<f64>,
    eval_times: Vec<f64>,
    y: Vec<f64>,
    param_values: Vec<f64>,
    n_x: usize,
    all_names: Vec<String>,
    visible_name_set: HashSet<String>,
    solver_name_to_idx: HashMap<String, usize>,
    requires_projection: bool,
}

fn trace_projection_failed_at_time(t: f64) {
    if sim_trace_enabled() {
        eprintln!("[sim-trace] no-state runtime projection failed at t={}", t);
    }
}

fn prepare_algebraic_result_setup(
    dae: &rumoca_ir_dae::Dae,
    opts: &SimOptions,
    elim: &eliminate::EliminationResult,
    budget: &crate::TimeoutBudget,
    visible_names: Vec<String>,
    dummy_state_name: &str,
) -> Result<AlgebraicResultSetup, SimError> {
    let dt = opts.dt.unwrap_or(opts.t_end / 500.0);
    let times = timeline::build_output_times(opts.t_start, opts.t_end, dt);
    let n_total = dae.f_x.len();
    let n_x = count_states(dae);

    let mut y = vec![0.0; n_total];
    initialize_state_vector(dae, &mut y);

    let param_values = default_params_with_budget(dae, budget)?;
    dump_parameter_vector(dae, &param_values);
    let clock_events =
        timeline::collect_periodic_clock_events(&dae.clock_schedules, opts.t_start, opts.t_end);
    if sim_introspect_enabled() {
        let preview: Vec<f64> = clock_events.iter().copied().take(12).collect();
        eprintln!(
            "[sim-introspect] no-state clock events count={} preview={:?}",
            clock_events.len(),
            preview
        );
    }
    let eval_times = timeline::merge_evaluation_times(&times, &clock_events);
    let mut all_names = visible_names.clone();
    all_names.extend(crate::collect_reconstruction_discrete_context_names(
        dae, elim, &all_names,
    ));
    let visible_name_set: HashSet<String> = visible_names
        .iter()
        .filter(|name| *name != dummy_state_name)
        .cloned()
        .collect();
    let mut solver_names = visible_names;
    solver_names.truncate(n_total);
    let solver_name_to_idx: HashMap<String, usize> = solver_names
        .iter()
        .enumerate()
        .map(|(idx, name)| (name.clone(), idx))
        .collect();
    let requires_projection = runtime_projection_required(dae, n_x);
    if sim_trace_enabled() {
        eprintln!(
            "[sim-trace] no-state runtime projection required={}",
            requires_projection
        );
    }

    Ok(AlgebraicResultSetup {
        times,
        eval_times,
        y,
        param_values,
        n_x,
        all_names,
        visible_name_set,
        solver_name_to_idx,
        requires_projection,
    })
}

fn collect_no_state_sample_data(
    dae: &rumoca_ir_dae::Dae,
    opts: &SimOptions,
    elim: &eliminate::EliminationResult,
    budget: &crate::TimeoutBudget,
    setup: &AlgebraicResultSetup,
) -> Result<Vec<Vec<f64>>, SimError> {
    let sample_ctx = NoStateSampleContext {
        dae,
        elim,
        param_values: &setup.param_values,
        all_names: &setup.all_names,
        solver_name_to_idx: &setup.solver_name_to_idx,
        n_x: setup.n_x,
        t_start: opts.t_start,
        requires_projection: setup.requires_projection,
    };

    let (_, data) = crate::collect_algebraic_samples(
        &sample_ctx,
        &setup.times,
        &setup.eval_times,
        setup.y.clone(),
        || budget.check().map_err(SimError::from),
        |y_values, t, do_projection| {
            if do_projection {
                let projection = project_algebraics_with_fixed_states_at_time(
                    dae,
                    y_values,
                    setup.n_x,
                    t,
                    opts.atol.max(1.0e-8),
                    budget,
                )?;
                if let Some(projected) = projection {
                    *y_values = projected;
                    return Ok(());
                }
                trace_projection_failed_at_time(t);
            }
            let _ =
                seed_runtime_direct_assignments(dae, y_values, &setup.param_values, setup.n_x, t);
            Ok(())
        },
    )
    .map_err(|err| match err {
        crate::NoStateSampleError::Callback(sim_err) => sim_err,
        crate::NoStateSampleError::SampleScheduleMismatch { captured, expected } => {
            SimError::SolverError(format!(
                "no-state sample schedule mismatch: captured {captured}/{expected} output samples"
            ))
        }
    })?;

    Ok(data)
}

fn filter_visible_output_series(
    recon_names: &[String],
    recon_data: &[Vec<f64>],
    visible_name_set: &HashSet<String>,
) -> (Vec<String>, Vec<Vec<f64>>) {
    let mut final_names: Vec<String> = Vec::new();
    let mut final_data: Vec<Vec<f64>> = Vec::new();
    for (name, series) in recon_names.iter().zip(recon_data.iter()) {
        if visible_name_set.contains(name) {
            final_names.push(name.clone());
            final_data.push(series.clone());
        }
    }
    (final_names, final_data)
}

pub fn build_algebraic_result(
    dae: &rumoca_ir_dae::Dae,
    opts: &SimOptions,
    elim: &eliminate::EliminationResult,
    budget: &crate::TimeoutBudget,
    visible_names: Vec<String>,
    dummy_state_name: &str,
) -> Result<SimResult, SimError> {
    let setup =
        prepare_algebraic_result_setup(dae, opts, elim, budget, visible_names, dummy_state_name)?;
    let data = collect_no_state_sample_data(dae, opts, elim, budget, &setup)?;
    let (recon_names, recon_data, final_n_states) =
        finalize_algebraic_outputs(setup.all_names, data, setup.n_x, dummy_state_name);
    let (mut final_names, mut final_data) =
        filter_visible_output_series(&recon_names, &recon_data, &setup.visible_name_set);

    if !elim.substitutions.is_empty() {
        let (extra_names, extra_data) = reconstruct::reconstruct_eliminated(
            elim,
            dae,
            &setup.param_values,
            &setup.times,
            &recon_names,
            &recon_data,
        );
        final_names.extend(extra_names);
        final_data.extend(extra_data);
    }

    let variable_meta = crate::build_variable_meta(dae, &final_names, final_n_states);
    Ok(SimResult {
        times: setup.times,
        names: final_names,
        data: final_data,
        n_states: final_n_states,
        variable_meta,
    })
}

pub fn finalize_dynamic_result(
    dae: &rumoca_ir_dae::Dae,
    elim: &eliminate::EliminationResult,
    param_values: &[f64],
    n_x: usize,
    output_names: Vec<String>,
    buf: OutputBuffers,
) -> SimResult {
    let solver_names = output_names.clone();
    let OutputBuffers {
        times: output_times,
        data: output_data,
        n_total: _,
        runtime_names,
        runtime_data,
    } = buf;
    let (mut final_names, mut final_data, final_n_states) = (output_names, output_data, n_x);
    let runtime_capture_complete =
        !runtime_names.is_empty() && runtime_data.iter().all(|s| s.len() == output_times.len());
    if runtime_capture_complete {
        crate::merge_runtime_discrete_channels(
            &mut final_names,
            &mut final_data,
            runtime_names,
            runtime_data,
        );
    }
    let (discrete_names, discrete_data) = crate::evaluate_runtime_discrete_channels(
        dae,
        n_x,
        param_values,
        &output_times,
        &solver_names,
        &final_data,
    );
    crate::merge_runtime_discrete_channels(
        &mut final_names,
        &mut final_data,
        discrete_names,
        discrete_data,
    );
    if !elim.substitutions.is_empty() {
        let (extra_names, extra_data) = reconstruct::reconstruct_eliminated(
            elim,
            dae,
            param_values,
            &output_times,
            &final_names,
            &final_data,
        );
        final_names.extend(extra_names);
        final_data.extend(extra_data);
    }
    let variable_meta = crate::build_variable_meta(dae, &final_names, final_n_states);
    SimResult {
        times: output_times,
        names: final_names,
        data: final_data,
        n_states: final_n_states,
        variable_meta,
    }
}
