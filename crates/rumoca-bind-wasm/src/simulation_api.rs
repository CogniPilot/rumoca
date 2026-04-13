use rumoca_session::Session;
use rumoca_session::runtime::{SimOptions, simulate_dae};
use rumoca_sim::results_web::{
    SimulationRequestSummary, SimulationRunMetrics, build_simulation_metrics_value,
    build_simulation_payload,
};
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

    let dt_opt = if dt > 0.0 { Some(dt) } else { None };
    let opts = SimOptions {
        t_end,
        dt: dt_opt,
        ..SimOptions::default()
    };
    let sim_started = wasm_timing_start();
    let sim = simulate_dae(&result.dae, &opts)
        .map_err(|e| JsValue::from_str(&format!("Simulation error: {}", e)))?;
    let metrics = SimulationRunMetrics {
        simulate_seconds: Some(wasm_elapsed_ms(sim_started) as f64 / 1000.0),
        ..SimulationRunMetrics::default()
    };
    let request = SimulationRequestSummary {
        solver: if solver.trim().is_empty() {
            "auto".to_string()
        } else {
            solver.trim().to_string()
        },
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
