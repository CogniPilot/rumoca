mod options;

use options::InteractiveOptions;
use wasm_bindgen::prelude::*;

use crate::{
    WasmError, compile_requested_model, qualify_input_model_name,
    simulation_api::build_simulation_options, with_singleton_session,
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
    pub fn new(source: &str, model_name: &str) -> Result<WasmSimulationSession, WasmError> {
        Self::with_interactive_options(source, model_name, 0.0, "", 0.0, 0.0, "[]")
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
        initial_inputs_json: &str,
    ) -> Result<WasmSimulationSession, WasmError> {
        let options = InteractiveOptions {
            dt,
            solver: solver.into(),
            atol,
            rtol,
            ..Default::default()
        };
        create_interactive_session(source, model_name, &options, || {
            serde_json::from_str(initial_inputs_json)
                .map_err(|e| WasmError::new(format!("Invalid initial inputs: {e}")))
        })
    }

    /// Construct an interactive session with checked options including the
    /// existing `auto` or `interpreter` execution policy. Empty options retain
    /// the model's experiment metadata and existing automatic execution default.
    #[wasm_bindgen(js_name = withInteractiveConfiguration)]
    pub fn with_interactive_configuration(
        source: &str,
        model_name: &str,
        options_json: &str,
    ) -> Result<WasmSimulationSession, WasmError> {
        let mut options = InteractiveOptions::parse(options_json)?;
        let initial_inputs = std::mem::take(&mut options.initial_inputs);
        create_interactive_session(source, model_name, &options, || Ok(initial_inputs))
    }

    /// Set an input value by name. Takes effect on the next advance.
    pub fn set_input(&mut self, name: &str, value: f64) -> Result<(), WasmError> {
        self.session
            .set_input(name, value)
            .map_err(|e| WasmError::new(format!("{e}")))
    }

    /// Apply one atomic input frame encoded as `[["name", value], ...]`.
    pub fn set_inputs(&mut self, inputs_json: &str) -> Result<(), WasmError> {
        let inputs: Vec<(String, f64)> = serde_json::from_str(inputs_json)
            .map_err(|e| WasmError::new(format!("Invalid input frame: {e}")))?;
        let batch: Vec<_> = inputs
            .iter()
            .map(|(name, value)| (name.as_str(), *value))
            .collect();
        self.session
            .set_inputs(&batch)
            .map_err(|e| WasmError::new(format!("{e}")))
    }

    /// Advance the simulation to an absolute target time in seconds.
    pub fn advance_to(&mut self, target_time: f64) -> Result<(), WasmError> {
        self.session.ensure_end_time(target_time);
        self.session
            .advance_to(target_time)
            .map_err(|e| WasmError::new(format!("Advance error: {e}")))
    }

    /// Advance the simulation by a relative time step in seconds.
    pub fn step(&mut self, dt: f64) -> Result<(), WasmError> {
        self.session.ensure_end_time(self.session.time() + dt);
        self.session
            .step(dt)
            .map_err(|e| WasmError::new(format!("Step error: {e}")))
    }

    /// Get the current simulation time.
    pub fn time(&self) -> f64 {
        self.session.time()
    }

    /// Read a single variable value by name.
    pub fn get(&self, name: &str) -> Result<Option<f64>, WasmError> {
        self.session
            .get(name)
            .map_err(|e| WasmError::new(format!("Session read error: {e}")))
    }

    /// Get all current variable values as a JSON string `{"time": t, "values": {...}}`.
    pub fn state_json(&self) -> Result<String, WasmError> {
        let state = self
            .session
            .state()
            .map_err(|e| WasmError::new(format!("Session state error: {e}")))?;
        serde_json::to_string(&serde_json::json!({
            "time": state.time,
            "values": state.values,
        }))
        .map_err(|e| WasmError::new(format!("Session state serialization error: {e}")))
    }

    /// Get available input names as a JSON array string.
    pub fn input_names(&self) -> Result<String, WasmError> {
        serde_json::to_string(self.session.input_names())
            .map_err(|e| WasmError::new(format!("Input name serialization error: {e}")))
    }

    /// Get all solver variable names as a JSON array string.
    pub fn variable_names(&self) -> Result<String, WasmError> {
        serde_json::to_string(self.session.variable_names())
            .map_err(|e| WasmError::new(format!("Variable name serialization error: {e}")))
    }

    /// Reset the simulation to initial conditions.
    pub fn reset(&mut self) -> Result<(), WasmError> {
        self.reset_at(0.0)
    }

    /// Restart the original Modelica initialization at an absolute time.
    /// Invalid restart coordinates leave the existing session untouched.
    pub fn reset_at(&mut self, time: f64) -> Result<(), WasmError> {
        if !time.is_finite() || time < 0.0 {
            return Err(WasmError::new("Reset time must be finite and nonnegative"));
        }
        self.session
            .reset(time)
            .map_err(|e| WasmError::new(format!("Reset failed: {e}")))?;
        Ok(())
    }
}

fn create_interactive_session(
    source: &str,
    model_name: &str,
    options: &InteractiveOptions,
    initial_inputs: impl FnOnce() -> Result<Vec<(String, f64)>, WasmError>,
) -> Result<WasmSimulationSession, WasmError> {
    let (dae, mut opts) = with_singleton_session(|session| {
        session.update_document("input.mo", source);
        let requested_model = qualify_input_model_name(session, model_name);
        let result = compile_requested_model(session, &requested_model)?;
        let (opts, _) = build_simulation_options(&result, 0.0, options.dt, &options.solver);
        Ok((result.dae, opts))
    })?;
    if options.atol.is_finite() && options.atol > 0.0 {
        opts.atol = options.atol;
    }
    if options.rtol.is_finite() && options.rtol > 0.0 {
        opts.rtol = options.rtol;
    }
    opts.initial_inputs = initial_inputs()?;
    opts.execution_policy = options.execution_policy;
    let session = rumoca_sim::SimulationSession::new(&dae, opts)
        .map_err(|e| WasmError::new(format!("Session creation error: {e}")))?;
    Ok(WasmSimulationSession { session })
}
