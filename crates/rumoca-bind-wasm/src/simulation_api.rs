use rumoca_compile::{Session, compile::CompilationResult};
use rumoca_sim::{
    SimOptions, SimResult, SimSolverMode, SimulationRequestSummary, SimulationRunMetrics,
    build_simulation_metrics_value, build_simulation_payload, build_tunable_parameter_meta,
    lower_dae_for_simulation, lower_for_simulation_with_overrides, refresh_prepared_vectors,
    simulate_dae_with_diagnostics, simulate_solve_model,
};
use wasm_bindgen::JsValue;

use crate::{
    compile_requested_model, qualify_input_model_name,
    source_root_api::{load_source_root_sources_in_session, load_workspace_sources_for_simulation},
    wasm_elapsed_ms, wasm_timing_start, with_singleton_session,
};

pub(crate) fn simulate_model_impl(
    source: &str,
    model_name: &str,
    t_end: f64,
    dt: f64,
    solver: &str,
    parameter_overrides_json: &str,
) -> Result<String, JsValue> {
    with_singleton_session(|session| {
        simulate_model_in_session(
            session,
            source,
            model_name,
            t_end,
            dt,
            solver,
            parameter_overrides_json,
        )
    })
}

/// Compile a model and emit a `{ solve_model, t_end, dt }` payload for the lazy
/// diffsol addon (`@cognipilot/rumoca/diffsol`). Lowered with the diffsol
/// (`bdf`) solver mode so the structure matches what the addon runs. The
/// resolved `t_end`/`dt` (from the experiment annotation when the caller passes
/// 0) travel with the model, since the `SolveModel` does not carry them. The
/// main module stays SIMD-free; only the addon that consumes this carries
/// relaxed-SIMD.
pub(crate) fn lower_model_to_solve_json_impl(
    source: &str,
    model_name: &str,
    t_end: f64,
    dt: f64,
    parameter_overrides_json: &str,
) -> Result<String, JsValue> {
    with_singleton_session(|session| {
        session.update_document("input.mo", source);
        let requested_model = qualify_input_model_name(session, model_name);
        let result = compile_requested_model(session, &requested_model)?;
        let (opts, _solver_label) = build_simulation_options(&result, t_end, dt, "bdf");
        let parameter_overrides = parse_parameter_overrides(parameter_overrides_json)?;
        let solve_model = lower_solve_model_with_overrides(&result, &opts, &parameter_overrides)?;
        let payload = serde_json::json!({
            "solve_model": solve_model,
            "t_end": opts.t_end,
            "dt": opts.dt.unwrap_or(0.0),
        });
        serde_json::to_string(&payload).map_err(|e| JsValue::from_str(&format!("JSON error: {e}")))
    })
}

pub(crate) fn simulate_model_with_workspace_sources_impl(
    source: &str,
    model_name: &str,
    workspace_sources_json: &str,
    t_end: f64,
    dt: f64,
    solver: &str,
    parameter_overrides_json: &str,
) -> Result<String, JsValue> {
    with_singleton_session(|session| {
        load_workspace_sources_for_simulation(session, workspace_sources_json)?;
        simulate_model_in_session(
            session,
            source,
            model_name,
            t_end,
            dt,
            solver,
            parameter_overrides_json,
        )
    })
}

pub(crate) fn simulate_model_with_source_roots_impl(
    source: &str,
    model_name: &str,
    source_roots_json: &str,
    t_end: f64,
    dt: f64,
    solver: &str,
    parameter_overrides_json: &str,
) -> Result<String, JsValue> {
    with_singleton_session(|session| {
        load_source_root_sources_in_session(session, source_roots_json)?;
        simulate_model_in_session(
            session,
            source,
            model_name,
            t_end,
            dt,
            solver,
            parameter_overrides_json,
        )
    })
}

pub(crate) fn model_parameter_metadata_impl(
    source: &str,
    model_name: &str,
) -> Result<String, JsValue> {
    with_singleton_session(|session| {
        model_parameter_metadata_in_session(session, source, model_name)
    })
}

pub(crate) fn model_parameter_metadata_with_workspace_sources_impl(
    source: &str,
    model_name: &str,
    workspace_sources_json: &str,
) -> Result<String, JsValue> {
    with_singleton_session(|session| {
        load_workspace_sources_for_simulation(session, workspace_sources_json)?;
        model_parameter_metadata_in_session(session, source, model_name)
    })
}

pub(crate) fn model_parameter_metadata_with_source_roots_impl(
    source: &str,
    model_name: &str,
    source_roots_json: &str,
) -> Result<String, JsValue> {
    with_singleton_session(|session| {
        load_source_root_sources_in_session(session, source_roots_json)?;
        model_parameter_metadata_in_session(session, source, model_name)
    })
}

fn simulate_model_in_session(
    session: &mut Session,
    source: &str,
    model_name: &str,
    t_end: f64,
    dt: f64,
    solver: &str,
    parameter_overrides_json: &str,
) -> Result<String, JsValue> {
    session.update_document("input.mo", source);
    let requested_model = qualify_input_model_name(session, model_name);
    let result = compile_requested_model(session, &requested_model)?;

    let (opts, solver_label) = build_simulation_options(&result, t_end, dt, solver);
    let parameter_overrides = parse_parameter_overrides(parameter_overrides_json)?;
    let sim_started = wasm_timing_start();
    let sim = run_simulation(&result, &opts, &parameter_overrides)?;
    let metrics = SimulationRunMetrics {
        simulate_seconds: Some(wasm_elapsed_ms(sim_started) as f64 / 1000.0),
        ..SimulationRunMetrics::default()
    };
    let request = SimulationRequestSummary {
        solver: solver_label,
        t_start: opts.t_start,
        t_end: opts.t_end,
        dt: opts.dt,
        rtol: opts.rtol,
        atol: opts.atol,
    };

    let output = serde_json::json!({
        "payload": build_simulation_payload(&sim, &request, &metrics),
        "metrics": build_simulation_metrics_value(&sim, &metrics),
    });
    serde_json::to_string(&output).map_err(|e| JsValue::from_str(&format!("JSON error: {}", e)))
}

fn run_simulation(
    result: &CompilationResult,
    opts: &SimOptions,
    parameter_overrides: &[(String, f64)],
) -> Result<SimResult, JsValue> {
    if parameter_overrides.is_empty() {
        return simulate_dae_with_diagnostics(&result.dae, opts)
            .map_err(|error| JsValue::from_str(&format!("Simulation error: {error}")));
    }
    let solve_model = lower_solve_model_with_overrides(result, opts, parameter_overrides)?;
    simulate_solve_model(&solve_model, opts)
        .map_err(|error| JsValue::from_str(&format!("Simulation error: {error}")))
}

fn model_parameter_metadata_in_session(
    session: &mut Session,
    source: &str,
    model_name: &str,
) -> Result<String, JsValue> {
    session.update_document("input.mo", source);
    let requested_model = qualify_input_model_name(session, model_name);
    let result = compile_requested_model(session, &requested_model)?;
    let (opts, _) = build_simulation_options(&result, 0.0, 0.0, "auto");
    let solve_model = lower_dae_for_simulation(&result.dae, &opts)
        .map_err(|e| JsValue::from_str(&format!("solve lowering error: {e}")))?;
    serde_json::to_string(&build_tunable_parameter_meta(&result.dae, &solve_model))
        .map_err(|e| JsValue::from_str(&format!("JSON error: {e}")))
}

fn lower_solve_model_with_overrides(
    result: &CompilationResult,
    opts: &SimOptions,
    parameter_overrides: &[(String, f64)],
) -> Result<rumoca_ir_solve::SolveModel, JsValue> {
    let mut override_opts = opts.clone();
    override_opts.param_overrides = parameter_overrides.to_vec();
    let mut solve_model = lower_for_simulation_with_overrides(&result.dae, &override_opts)
        .map_err(|e| JsValue::from_str(&format!("solve lowering error: {e}")))?;
    if !parameter_overrides.is_empty() {
        let (initial_y, parameters) =
            refresh_prepared_vectors(&solve_model, opts.t_start, parameter_overrides)
                .map_err(|e| JsValue::from_str(&format!("parameter override error: {e}")))?;
        solve_model.initial_y = initial_y;
        solve_model.parameters = parameters;
    }
    Ok(solve_model)
}

fn parse_parameter_overrides(json: &str) -> Result<Vec<(String, f64)>, JsValue> {
    let trimmed = json.trim();
    if trimmed.is_empty() || trimmed == "{}" || trimmed == "null" {
        return Ok(Vec::new());
    }
    let parsed: serde_json::Value = serde_json::from_str(trimmed)
        .map_err(|e| JsValue::from_str(&format!("parameter override JSON error: {e}")))?;
    let Some(object) = parsed.as_object() else {
        return Err(JsValue::from_str(
            "parameter overrides must be a JSON object",
        ));
    };
    let mut overrides = Vec::with_capacity(object.len());
    for (name, value) in object {
        let Some(value) = value.as_f64() else {
            return Err(JsValue::from_str(&format!(
                "parameter override `{name}` must be numeric"
            )));
        };
        if !value.is_finite() {
            return Err(JsValue::from_str(&format!(
                "parameter override `{name}` is not finite"
            )));
        }
        overrides.push((name.clone(), value));
    }
    Ok(overrides)
}

pub(crate) fn build_simulation_options(
    result: &CompilationResult,
    t_end: f64,
    dt: f64,
    solver: &str,
) -> (SimOptions, String) {
    let solver_request = if solver.trim().is_empty() {
        result.experiment_solver.as_deref().unwrap_or(solver)
    } else {
        solver
    };
    let (solver_mode, solver_label) = parse_wasm_solver_request(solver_request);
    let defaults = SimOptions::default();
    let t_start = result
        .experiment_start_time
        .filter(|value| value.is_finite())
        .unwrap_or(defaults.t_start);
    let mut resolved_t_end = if t_end > 0.0 {
        t_end
    } else {
        result
            .experiment_stop_time
            .filter(|value| value.is_finite())
            .unwrap_or(defaults.t_end)
    };
    if resolved_t_end <= t_start {
        resolved_t_end = t_start + 1.0;
    }
    let tolerance = result
        .experiment_tolerance
        .filter(|value| value.is_finite() && *value > 0.0);
    let dt_opt = if dt > 0.0 {
        Some(dt)
    } else {
        result
            .experiment_interval
            .filter(|value| value.is_finite() && *value > 0.0)
    };
    (
        SimOptions {
            t_start,
            t_end: resolved_t_end,
            rtol: tolerance.unwrap_or(defaults.rtol),
            atol: tolerance.unwrap_or(defaults.atol),
            dt: dt_opt,
            solver_mode,
            ..defaults
        },
        solver_label,
    )
}

fn parse_wasm_solver_request(solver: &str) -> (SimSolverMode, String) {
    let (solver_mode, solver_label) = SimSolverMode::parse_request(Some(solver));
    if solver_mode == SimSolverMode::Auto {
        resolve_wasm_auto_solver(solver_label)
    } else {
        (solver_mode, solver_label)
    }
}

#[cfg(feature = "sim-rk45")]
fn resolve_wasm_auto_solver(solver_label: String) -> (SimSolverMode, String) {
    (SimSolverMode::RkLike, solver_label)
}

#[cfg(not(feature = "sim-rk45"))]
fn resolve_wasm_auto_solver(solver_label: String) -> (SimSolverMode, String) {
    (SimSolverMode::Auto, solver_label)
}
