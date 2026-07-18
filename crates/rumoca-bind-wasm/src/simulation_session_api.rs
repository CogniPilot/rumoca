use wasm_bindgen::prelude::*;

use crate::{
    compile_requested_model, qualify_input_model_name, simulation_api::build_simulation_options,
    with_singleton_session,
};

/// Opaque handle to a real-time simulation session running in WASM.
///
/// Compiles a Modelica model and creates an interactive session that can be
/// driven from JavaScript via `requestAnimationFrame`.
#[wasm_bindgen]
pub struct WasmSimulationSession {
    session: rumoca_sim::SimulationSession,
}

#[wasm_bindgen]
impl WasmSimulationSession {
    /// Compile a Modelica model and create a session ready for interactive use.
    ///
    /// `source` is the full Modelica source text and `model_name` is the class
    /// to simulate. The model's experiment metadata supplies default simulation
    /// options.
    #[wasm_bindgen(constructor)]
    pub fn new(source: &str, model_name: &str) -> Result<WasmSimulationSession, JsValue> {
        Self::with_interactive_options(source, model_name, 0.0, "", 0.0, 0.0)
    }

    /// Compile a Modelica model and create a user-terminated interactive session.
    #[wasm_bindgen(js_name = withInteractiveOptions)]
    pub fn with_interactive_options(
        source: &str,
        model_name: &str,
        dt: f64,
        solver: &str,
        atol: f64,
        rtol: f64,
    ) -> Result<WasmSimulationSession, JsValue> {
        let (dae, mut opts) = with_singleton_session(|session| {
            session.update_document("input.mo", source);
            let requested_model = qualify_input_model_name(session, model_name);
            let result = compile_requested_model(session, &requested_model)?;
            let (opts, _solver_label) = build_simulation_options(&result, 0.0, dt, solver);
            Ok((result.dae, opts))
        })?;
        if atol.is_finite() && atol > 0.0 {
            opts.atol = atol;
        }
        if rtol.is_finite() && rtol > 0.0 {
            opts.rtol = rtol;
        }

        let session = rumoca_sim::SimulationSession::new(&dae, opts)
            .map_err(|e| JsValue::from_str(&format!("Session creation error: {e}")))?;

        Ok(WasmSimulationSession { session })
    }

    /// Set an input value by name. Takes effect on the next advance.
    pub fn set_input(&mut self, name: &str, value: f64) -> Result<(), JsValue> {
        self.session
            .set_input(name, value)
            .map_err(|e| JsValue::from_str(&format!("{e}")))
    }

    /// Advance the simulation to an absolute target time in seconds.
    pub fn advance_to(&mut self, target_time: f64) -> Result<(), JsValue> {
        self.session.ensure_end_time(target_time);
        self.session
            .advance_to(target_time)
            .map_err(|e| JsValue::from_str(&format!("Advance error: {e}")))
    }

    /// Advance the simulation by a relative time step in seconds.
    pub fn step(&mut self, dt: f64) -> Result<(), JsValue> {
        self.session.ensure_end_time(self.session.time() + dt);
        self.session
            .step(dt)
            .map_err(|e| JsValue::from_str(&format!("Step error: {e}")))
    }

    /// Get the current simulation time.
    pub fn time(&self) -> f64 {
        self.session.time()
    }

    /// Read a single variable value by name.
    pub fn get(&self, name: &str) -> Result<Option<f64>, JsValue> {
        self.session
            .get(name)
            .map_err(|e| JsValue::from_str(&format!("Session read error: {e}")))
    }

    /// Get all current variable values as a JSON string `{"time": t, "values": {...}}`.
    pub fn state_json(&self) -> Result<String, JsValue> {
        let state = self
            .session
            .state()
            .map_err(|e| JsValue::from_str(&format!("Session state error: {e}")))?;
        serde_json::to_string(&serde_json::json!({
            "time": state.time,
            "values": state.values,
        }))
        .map_err(|e| JsValue::from_str(&format!("Session state serialization error: {e}")))
    }

    /// Get available input names as a JSON array string.
    pub fn input_names(&self) -> Result<String, JsValue> {
        serde_json::to_string(self.session.input_names())
            .map_err(|e| JsValue::from_str(&format!("Input name serialization error: {e}")))
    }

    /// Get all solver variable names as a JSON array string.
    pub fn variable_names(&self) -> Result<String, JsValue> {
        serde_json::to_string(self.session.variable_names())
            .map_err(|e| JsValue::from_str(&format!("Variable name serialization error: {e}")))
    }

    /// Reset the simulation to initial conditions.
    pub fn reset(&mut self) -> Result<(), JsValue> {
        self.session
            .reset(0.0)
            .map_err(|e| JsValue::from_str(&format!("Reset failed: {e}")))?;
        Ok(())
    }
}
