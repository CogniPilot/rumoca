mod options;

use options::InteractiveOptions;
use rumoca_sim::{SESSION_PROTOCOL_VERSION, SessionCommand, SessionEvent};
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
        self.apply(SessionCommand::SetInput {
            name: name.to_owned(),
            value,
        })
        .map(drop)
    }

    /// Apply one atomic input frame encoded as `[["name", value], ...]`.
    pub fn set_inputs(&mut self, inputs_json: &str) -> Result<(), WasmError> {
        let inputs: Vec<(String, f64)> = serde_json::from_str(inputs_json)
            .map_err(|e| WasmError::new(format!("Invalid input frame: {e}")))?;
        self.apply(SessionCommand::SetInputs { inputs }).map(drop)
    }

    /// Advance the simulation to an absolute target time in seconds.
    pub fn advance_to(&mut self, target_time: f64) -> Result<(), WasmError> {
        self.apply(SessionCommand::AdvanceTo { time: target_time })
            .map(drop)
    }

    /// Advance the simulation by a relative time step in seconds.
    pub fn step(&mut self, dt: f64) -> Result<(), WasmError> {
        self.apply(SessionCommand::Step { dt }).map(drop)
    }

    /// Get the current simulation time.
    pub fn time(&self) -> f64 {
        self.session.time()
    }

    /// Read a single variable value by name.
    pub fn get(&mut self, name: &str) -> Result<Option<f64>, WasmError> {
        match self.apply(SessionCommand::Get {
            name: name.to_owned(),
        })? {
            SessionEvent::Value { value, .. } => Ok(value),
            other => Err(unexpected_event("get", &other)),
        }
    }

    /// Get all current variable values as a JSON string `{"time": t, "values": {...}}`.
    pub fn state_json(&mut self) -> Result<String, WasmError> {
        match self.apply(SessionCommand::State)? {
            SessionEvent::State { time, values } => {
                serde_json::to_string(&serde_json::json!({ "time": time, "values": values }))
                    .map_err(|e| WasmError::new(format!("Session state serialization error: {e}")))
            }
            other => Err(unexpected_event("state", &other)),
        }
    }

    /// The execution engine the session selected, as the JSON receipt
    /// `{"engine": "interpreter" | "cranelift" | "wasm_program", "refusal":
    /// null | "no_continuous_states" | "external_tables"}`. It is the engine
    /// field of the protocol `hello` event.
    pub fn execution_receipt_json(&mut self) -> Result<String, WasmError> {
        match self.apply(SessionCommand::Hello {
            protocol_version: SESSION_PROTOCOL_VERSION,
        })? {
            SessionEvent::Hello { engine, .. } => serde_json::to_string(&engine)
                .map_err(|e| WasmError::new(format!("Execution receipt serialization error: {e}"))),
            other => Err(unexpected_event("hello", &other)),
        }
    }

    /// Get available input names as a JSON array string.
    pub fn input_names(&mut self) -> Result<String, WasmError> {
        match self.apply(SessionCommand::InputNames)? {
            SessionEvent::InputNames { names } => names_json(&names),
            other => Err(unexpected_event("input_names", &other)),
        }
    }

    /// Get all solver variable names as a JSON array string.
    pub fn variable_names(&mut self) -> Result<String, WasmError> {
        match self.apply(SessionCommand::VariableNames)? {
            SessionEvent::VariableNames { names } => names_json(&names),
            other => Err(unexpected_event("variable_names", &other)),
        }
    }

    /// Reset the simulation to initial conditions.
    pub fn reset(&mut self) -> Result<(), WasmError> {
        self.reset_at(0.0)
    }

    /// Restart the original Modelica initialization at an absolute time.
    /// Invalid restart coordinates leave the existing session untouched.
    pub fn reset_at(&mut self, time: f64) -> Result<(), WasmError> {
        self.apply(SessionCommand::Reset { time: Some(time) })
            .map(drop)
    }
}

impl WasmSimulationSession {
    /// Send one command through the session dispatcher, surfacing an `error`
    /// event as the binding's error.
    fn apply(&mut self, command: SessionCommand) -> Result<SessionEvent, WasmError> {
        match self.session.apply(command) {
            SessionEvent::Error { code, message } => {
                Err(WasmError::new(format!("[{code}] {message}")))
            }
            event => Ok(event),
        }
    }
}

pub(crate) fn unexpected_event(command: &str, event: &SessionEvent) -> WasmError {
    WasmError::new(format!(
        "Session `{command}` returned unexpected event {event:?}"
    ))
}

fn names_json(names: &[String]) -> Result<String, WasmError> {
    serde_json::to_string(names)
        .map_err(|e| WasmError::new(format!("Session name serialization error: {e}")))
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
