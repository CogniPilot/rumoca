use wasm_bindgen::prelude::*;

use crate::{compile_requested_model, qualify_input_model_name, with_singleton_session};

/// Opaque handle to a real-time simulation stepper running in WASM.
///
/// Compiles a Modelica model and creates an interactive stepper that can be
/// driven from JavaScript via `requestAnimationFrame`.
#[wasm_bindgen]
pub struct WasmStepper {
    stepper: rumoca_sim_diffsol::SimStepper,
    /// Kept for `reset()` — recreates the stepper from scratch.
    dae: rumoca_session::compile::Dae,
}

#[wasm_bindgen]
impl WasmStepper {
    /// Compile a Modelica model and create a stepper ready for interactive stepping.
    ///
    /// `source` is the full Modelica source text, `model_name` is the class to simulate.
    #[wasm_bindgen(constructor)]
    pub fn new(source: &str, model_name: &str) -> Result<WasmStepper, JsValue> {
        let dae = with_singleton_session(|session| {
            session.update_document("input.mo", source);
            let requested_model = qualify_input_model_name(session, model_name);
            let result = compile_requested_model(session, &requested_model)?;
            Ok(result.dae)
        })?;

        let opts = rumoca_sim_diffsol::StepperOptions {
            rtol: 1e-3,
            atol: 1e-3,
            ..rumoca_sim_diffsol::StepperOptions::default()
        };
        let stepper = rumoca_sim_diffsol::SimStepper::new(&dae, opts)
            .map_err(|e| JsValue::from_str(&format!("Stepper creation error: {e}")))?;

        Ok(WasmStepper { stepper, dae })
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
    pub fn get(&self, name: &str) -> Option<f64> {
        self.stepper.get(name)
    }

    /// Get all current variable values as a JSON string `{"time": t, "values": {...}}`.
    pub fn state_json(&self) -> String {
        let state = self.stepper.state();
        serde_json::json!({
            "time": state.time,
            "values": state.values,
        })
        .to_string()
    }

    /// Get available input names as a JSON array string.
    pub fn input_names(&self) -> String {
        serde_json::to_string(self.stepper.input_names()).unwrap_or_else(|_| "[]".to_string())
    }

    /// Get all solver variable names as a JSON array string.
    pub fn variable_names(&self) -> String {
        serde_json::to_string(self.stepper.variable_names()).unwrap_or_else(|_| "[]".to_string())
    }

    /// Reset the simulation to initial conditions.
    pub fn reset(&mut self) -> Result<(), JsValue> {
        let opts = rumoca_sim_diffsol::StepperOptions {
            rtol: 1e-3,
            atol: 1e-3,
            ..rumoca_sim_diffsol::StepperOptions::default()
        };
        self.stepper = rumoca_sim_diffsol::SimStepper::new(&self.dae, opts)
            .map_err(|e| JsValue::from_str(&format!("Reset failed: {e}")))?;
        Ok(())
    }
}
