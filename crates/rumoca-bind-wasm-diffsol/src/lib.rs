//! Lazy diffsol simulation addon for the `@cognipilot/rumoca` WASM package.
//!
//! The main `rumoca` module is SIMD-free and loads on every browser. Diffsol
//! (stiff/implicit) pulls relaxed-SIMD via faer/pulp, which a single combined
//! module would require at instantiation — hard-failing the whole package on
//! older browsers. So diffsol lives here, in a *separate* module that:
//!   1. carries no compiler — it deserializes a `SolveModel` the main module
//!      already lowered (the boundary is lossless; see the
//!      `solve_model_round_trip` test), and
//!   2. is loaded lazily by JS only after feature-detecting relaxed-SIMD, so an
//!      old browser never has to instantiate it (the stiff solver is simply
//!      greyed out in the UI instead of breaking the page).

use rumoca_ir_solve::SolveModel;
use rumoca_solver::{
    SimOptions, SimSolverMode, SimulationRequestSummary, SimulationRunMetrics,
    build_simulation_payload,
};
use serde::Deserialize;
use wasm_bindgen::prelude::*;

/// The payload the main module's `lower_model_to_solve_json` produces: the
/// lowered model plus the resolved simulation time (which the SolveModel does
/// not itself carry, and which the main module resolves from the experiment
/// annotation when the caller defers).
#[derive(Deserialize)]
struct DiffsolInput {
    solve_model: SolveModel,
    #[serde(default)]
    t_end: f64,
    #[serde(default)]
    dt: f64,
}

#[wasm_bindgen(start)]
pub fn start() {
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();
}

/// Simulate a pre-lowered model with the stiff/implicit diffsol backend.
/// `input_json` is the `{ solve_model, t_end, dt }` payload from the main
/// module's `lower_model_to_solve_json`. Returns the same `{ "payload": ... }`
/// JSON shape as the main module's `simulate_model`, so callers treat both
/// paths uniformly.
#[wasm_bindgen]
pub fn simulate_solve_model_diffsol(input_json: &str) -> Result<String, JsValue> {
    let input: DiffsolInput = serde_json::from_str(input_json)
        .map_err(|e| JsValue::from_str(&format!("invalid diffsol input JSON: {e}")))?;

    let defaults = SimOptions::default();
    let opts = SimOptions {
        solver_mode: SimSolverMode::Bdf,
        t_end: if input.t_end > 0.0 {
            input.t_end
        } else {
            defaults.t_end
        },
        dt: if input.dt > 0.0 { Some(input.dt) } else { None },
        ..defaults
    };

    let sim = rumoca_solver_diffsol::simulate(&input.solve_model, &opts)
        .map_err(|e| JsValue::from_str(&format!("diffsol simulation error: {e}")))?;

    let metrics = SimulationRunMetrics::default();
    let request = SimulationRequestSummary {
        solver: "bdf (diffsol)".to_string(),
        t_start: opts.t_start,
        t_end: opts.t_end,
        dt: opts.dt,
        rtol: opts.rtol,
        atol: opts.atol,
    };
    let output = serde_json::json!({
        "payload": build_simulation_payload(&sim, &request, &metrics),
    });
    serde_json::to_string(&output).map_err(|e| JsValue::from_str(&format!("JSON error: {e}")))
}
