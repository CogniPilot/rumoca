use rumoca_compile::{Session, compile::CompilationResult};
use rumoca_sim::{
    SimOptions, SimResult, SimSolverMode, SimulationRequestSummary, SimulationRunMetrics,
    build_simulation_metrics_value, build_simulation_payload, lower_dae_for_simulation,
    simulate_dae_with_diagnostics,
};
use wasm_bindgen::JsValue;

use crate::{
    compile_requested_model, qualify_input_model_name,
    source_root_api::load_project_sources_for_simulation, wasm_elapsed_ms, wasm_timing_start,
    with_singleton_session,
};

pub(crate) fn simulate_model_impl(
    source: &str,
    model_name: &str,
    t_end: f64,
    dt: f64,
    solver: &str,
) -> Result<String, JsValue> {
    with_singleton_session(|session| {
        simulate_model_in_session(session, source, model_name, t_end, dt, solver)
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
) -> Result<String, JsValue> {
    with_singleton_session(|session| {
        session.update_document("input.mo", source);
        let requested_model = qualify_input_model_name(session, model_name);
        let result = compile_requested_model(session, &requested_model)?;
        let (opts, _solver_label) = build_simulation_options(&result, t_end, dt, "bdf");
        let solve_model = lower_dae_for_simulation(&result.dae, &opts)
            .map_err(|e| JsValue::from_str(&format!("solve lowering error: {e}")))?;
        let payload = serde_json::json!({
            "solve_model": solve_model,
            "t_end": opts.t_end,
            "dt": opts.dt.unwrap_or(0.0),
        });
        serde_json::to_string(&payload).map_err(|e| JsValue::from_str(&format!("JSON error: {e}")))
    })
}

pub(crate) fn simulate_model_with_project_sources_impl(
    source: &str,
    model_name: &str,
    project_sources_json: &str,
    t_end: f64,
    dt: f64,
    solver: &str,
) -> Result<String, JsValue> {
    with_singleton_session(|session| {
        load_project_sources_for_simulation(session, project_sources_json)?;
        simulate_model_in_session(session, source, model_name, t_end, dt, solver)
    })
}

fn simulate_model_in_session(
    session: &mut Session,
    source: &str,
    model_name: &str,
    t_end: f64,
    dt: f64,
    solver: &str,
) -> Result<String, JsValue> {
    session.update_document("input.mo", source);
    let requested_model = qualify_input_model_name(session, model_name);
    let result = compile_requested_model(session, &requested_model)?;

    let (opts, solver_label) = build_simulation_options(&result, t_end, dt, solver);
    let sim_started = wasm_timing_start();
    let sim = run_simulation(&result.dae, &opts)?;
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
    dae: &rumoca_compile::compile::Dae,
    opts: &SimOptions,
) -> Result<SimResult, JsValue> {
    simulate_dae_with_diagnostics(dae, opts)
        .map_err(|error| JsValue::from_str(&format!("Simulation error: {error}")))
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
