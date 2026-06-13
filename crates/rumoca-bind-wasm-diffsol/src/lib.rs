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
use wasm_bindgen::prelude::*;

#[wasm_bindgen(start)]
pub fn start() {
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();
}

/// Simulate a pre-lowered model (the JSON the main module produced from its
/// `SolveModel`) with the stiff/implicit diffsol backend. Returns the same
/// `{ "payload": ... }` JSON shape as the main module's `simulate_model`, so
/// callers can treat both paths uniformly.
#[wasm_bindgen]
pub fn simulate_solve_model_diffsol(
    solve_json: &str,
    t_end: f64,
    dt: f64,
) -> Result<String, JsValue> {
    let model: SolveModel = serde_json::from_str(solve_json)
        .map_err(|e| JsValue::from_str(&format!("invalid solve-model JSON: {e}")))?;

    let defaults = SimOptions::default();
    let opts = SimOptions {
        solver_mode: SimSolverMode::Bdf,
        t_end: if t_end > 0.0 { t_end } else { defaults.t_end },
        dt: if dt > 0.0 { Some(dt) } else { None },
        ..defaults
    };

    let sim = rumoca_solver_diffsol::simulate(&model, &opts)
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
