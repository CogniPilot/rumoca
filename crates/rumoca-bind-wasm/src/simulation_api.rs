use rumoca_session::Session;
use rumoca_sim::{
    SimOptions, SimResult, SimSolverMode, SimulationRequestSummary, SimulationRunMetrics,
    build_simulation_metrics_value, build_simulation_payload,
};
#[cfg(feature = "sim-diffsol")]
use rumoca_sim_diffsol::simulate_dae as simulate_dae_diffsol;
#[cfg(feature = "sim-rk45")]
use rumoca_sim_rk45::simulate_dae as simulate_dae_rk45;
use wasm_bindgen::JsValue;

use crate::{
    compile_requested_model, qualify_input_model_name, wasm_elapsed_ms, wasm_timing_start,
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

pub(crate) fn simulate_model_with_project_sources_impl(
    source: &str,
    model_name: &str,
    project_sources_json: &str,
    t_end: f64,
    dt: f64,
    solver: &str,
) -> Result<String, JsValue> {
    with_singleton_session(|session| {
        crate::source_root_api::load_project_sources_in_session(session, project_sources_json)?;
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

    let (opts, solver_label) = build_simulation_options(t_end, dt, solver);
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
    dae: &rumoca_session::compile::Dae,
    opts: &SimOptions,
) -> Result<SimResult, JsValue> {
    match opts.solver_mode {
        SimSolverMode::Auto => simulate_with_default_backend(dae, opts),
        SimSolverMode::Bdf => simulate_with_diffsol(dae, opts),
        SimSolverMode::RkLike => simulate_with_rk45(dae, opts),
    }
}

#[cfg(feature = "sim-diffsol")]
fn simulate_with_default_backend(
    dae: &rumoca_session::compile::Dae,
    opts: &SimOptions,
) -> Result<SimResult, JsValue> {
    simulate_with_diffsol(dae, opts)
}

#[cfg(all(not(feature = "sim-diffsol"), feature = "sim-rk45"))]
fn simulate_with_default_backend(
    dae: &rumoca_session::compile::Dae,
    opts: &SimOptions,
) -> Result<SimResult, JsValue> {
    simulate_with_rk45(dae, opts)
}

#[cfg(feature = "sim-diffsol")]
fn simulate_with_diffsol(
    dae: &rumoca_session::compile::Dae,
    opts: &SimOptions,
) -> Result<SimResult, JsValue> {
    simulate_dae_diffsol(dae, opts)
        .map_err(|error| JsValue::from_str(&format!("Simulation error (diffsol): {}", error)))
}

#[cfg(not(feature = "sim-diffsol"))]
fn simulate_with_diffsol(
    _dae: &rumoca_session::compile::Dae,
    _opts: &SimOptions,
) -> Result<SimResult, JsValue> {
    Err(JsValue::from_str(
        "Simulation error: this WASM build does not include the diffsol backend; enable the `sim-diffsol` feature or request an RK-like solver",
    ))
}

#[cfg(feature = "sim-rk45")]
fn simulate_with_rk45(
    dae: &rumoca_session::compile::Dae,
    opts: &SimOptions,
) -> Result<SimResult, JsValue> {
    simulate_dae_rk45(dae, opts)
        .map_err(|error| JsValue::from_str(&format!("Simulation error (rk45): {}", error)))
}

#[cfg(not(feature = "sim-rk45"))]
fn simulate_with_rk45(
    _dae: &rumoca_session::compile::Dae,
    _opts: &SimOptions,
) -> Result<SimResult, JsValue> {
    Err(JsValue::from_str(
        "Simulation error: this WASM build does not include the RK45 backend; enable the `sim-rk45` feature or request `auto`/`bdf` when diffsol is available",
    ))
}

pub(crate) fn build_simulation_options(t_end: f64, dt: f64, solver: &str) -> (SimOptions, String) {
    let (solver_mode, solver_label) = SimSolverMode::parse_request(Some(solver));
    let dt_opt = if dt > 0.0 { Some(dt) } else { None };
    (
        SimOptions {
            t_end,
            dt: dt_opt,
            solver_mode,
            ..SimOptions::default()
        },
        solver_label,
    )
}
