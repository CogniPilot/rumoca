use wasm_bindgen::prelude::*;

use crate::{
    compile_requested_model, qualify_input_model_name, simulation_api::build_simulation_options,
    with_singleton_session,
};

/// Opaque handle to a real-time simulation stepper running in WASM.
///
/// Compiles a Modelica model and creates an interactive stepper that can be
/// driven from JavaScript via `requestAnimationFrame`.
#[wasm_bindgen]
pub struct WasmStepper {
    stepper: rumoca_sim::SimStepper,
}

#[wasm_bindgen]
impl WasmStepper {
    /// Compile a Modelica model and create a stepper ready for interactive stepping.
    ///
    /// `source` is the full Modelica source text and `model_name` is the class
    /// to simulate. The stepper backend is chosen by the compiled package and
    /// model experiment metadata, not by the batch simulation solver selector.
    #[wasm_bindgen(constructor)]
    pub fn new(source: &str, model_name: &str) -> Result<WasmStepper, JsValue> {
        let (dae, opts) = with_singleton_session(|session| {
            session.update_document("input.mo", source);
            let requested_model = qualify_input_model_name(session, model_name);
            let result = compile_requested_model(session, &requested_model)?;
            let (opts, _solver_label) = build_simulation_options(&result, 0.0, 0.0, "");
            Ok((result.dae, opts))
        })?;

        let stepper = rumoca_sim::SimStepper::new(&dae, opts.clone())
            .map_err(|e| JsValue::from_str(&format!("Stepper creation error: {e}")))?;

        Ok(WasmStepper { stepper })
    }

    /// Set an input value by name. Takes effect on the next `step()` call.
    pub fn set_input(&mut self, name: &str, value: f64) -> Result<(), JsValue> {
        self.stepper
            .set_input(name, value)
            .map_err(|e| JsValue::from_str(&format!("{e}")))
    }

    /// Step the simulation forward by `dt` seconds.
    pub fn step(&mut self, dt: f64) -> Result<(), JsValue> {
        self.stepper
            .step(dt)
            .map_err(|e| JsValue::from_str(&format!("Step error: {e}")))
    }

    /// Get the current simulation time.
    pub fn time(&self) -> f64 {
        self.stepper.time()
    }

    /// Read a single variable value by name.
    pub fn get(&self, name: &str) -> Result<Option<f64>, JsValue> {
        self.stepper
            .get(name)
            .map_err(|e| JsValue::from_str(&format!("Stepper read error: {e}")))
    }

    /// Get all current variable values as a JSON string `{"time": t, "values": {...}}`.
    pub fn state_json(&self) -> Result<String, JsValue> {
        let state = self
            .stepper
            .state()
            .map_err(|e| JsValue::from_str(&format!("Stepper state error: {e}")))?;
        serde_json::to_string(&serde_json::json!({
            "time": state.time,
            "values": state.values,
        }))
        .map_err(|e| JsValue::from_str(&format!("Stepper state serialization error: {e}")))
    }

    /// Get available input names as a JSON array string.
    pub fn input_names(&self) -> Result<String, JsValue> {
        serde_json::to_string(self.stepper.input_names())
            .map_err(|e| JsValue::from_str(&format!("Input name serialization error: {e}")))
    }

    /// Get all solver variable names as a JSON array string.
    pub fn variable_names(&self) -> Result<String, JsValue> {
        serde_json::to_string(self.stepper.variable_names())
            .map_err(|e| JsValue::from_str(&format!("Variable name serialization error: {e}")))
    }

    /// Reset the simulation to initial conditions.
    pub fn reset(&mut self) -> Result<(), JsValue> {
        self.stepper
            .reset(0.0)
            .map_err(|e| JsValue::from_str(&format!("Reset failed: {e}")))?;
        Ok(())
    }
}
