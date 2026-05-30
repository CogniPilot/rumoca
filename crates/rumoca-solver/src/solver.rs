//! Backend-neutral solver contracts shared by simulation backends.

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SimSolverMode {
    Auto,
    Bdf,
    RkLike,
}

impl SimSolverMode {
    pub fn from_external_name(name: &str) -> Self {
        let lower = name.trim().to_ascii_lowercase();
        if lower.is_empty() {
            return Self::Bdf;
        }
        if lower == "auto" {
            return Self::Auto;
        }

        let normalized = lower.replace(['-', '_', ' '], "");
        let rk_like = normalized.contains("rungekutta")
            || normalized.starts_with("rk")
            || normalized.contains("dopri")
            || normalized.contains("esdirk")
            || normalized.contains("trbdf2")
            || normalized.contains("euler")
            || normalized.contains("midpoint");

        if rk_like { Self::RkLike } else { Self::Bdf }
    }

    pub fn parse_request(solver: Option<&str>) -> (Self, String) {
        match solver {
            Some(raw) if !raw.trim().is_empty() => {
                let trimmed = raw.trim();
                (Self::from_external_name(trimmed), trimmed.to_string())
            }
            _ => (Self::Auto, "auto".to_string()),
        }
    }
}

#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum SimPacingMode {
    AsFastAsPossible,
    Realtime,
    Lockstep,
}

impl SimPacingMode {
    #[must_use]
    pub fn for_interactive_default(has_external_coupling: bool) -> Self {
        if has_external_coupling {
            Self::Lockstep
        } else {
            Self::Realtime
        }
    }

    #[must_use]
    pub fn resolve_interactive(explicit: Option<Self>, has_external_coupling: bool) -> Self {
        explicit.unwrap_or_else(|| Self::for_interactive_default(has_external_coupling))
    }

    #[must_use]
    pub fn label(self) -> &'static str {
        match self {
            Self::AsFastAsPossible => "as_fast_as_possible",
            Self::Realtime => "realtime",
            Self::Lockstep => "lockstep",
        }
    }
}

#[derive(Debug, Clone)]
pub struct SimOptions {
    pub t_start: f64,
    pub t_end: f64,
    pub rtol: f64,
    pub atol: f64,
    pub dt: Option<f64>,
    pub scalarize: bool,
    pub max_wall_seconds: Option<f64>,
    pub solver_mode: SimSolverMode,
    pub pacing_mode: SimPacingMode,
}

impl Default for SimOptions {
    fn default() -> Self {
        Self {
            t_start: 0.0,
            t_end: 1.0,
            rtol: 1e-6,
            atol: 1e-6,
            dt: None,
            scalarize: true,
            max_wall_seconds: None,
            solver_mode: SimSolverMode::Auto,
            pacing_mode: SimPacingMode::AsFastAsPossible,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SimBackend {
    Diffsol,
    Rk45,
}

#[derive(Debug, Clone)]
pub struct SimVariableMeta {
    pub name: String,
    pub role: String,
    pub is_state: bool,
    pub value_type: Option<String>,
    pub variability: Option<String>,
    pub time_domain: Option<String>,
    pub unit: Option<String>,
    pub start: Option<String>,
    pub min: Option<String>,
    pub max: Option<String>,
    pub nominal: Option<String>,
    pub fixed: Option<bool>,
    pub description: Option<String>,
}

#[derive(Debug, Clone)]
pub struct SimTermination {
    pub time: f64,
    pub message: String,
}

#[derive(Debug, Clone)]
pub struct SimResult {
    pub times: Vec<f64>,
    pub names: Vec<String>,
    pub data: Vec<Vec<f64>>,
    pub n_states: usize,
    pub variable_meta: Vec<SimVariableMeta>,
    pub termination: Option<SimTermination>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct BackendState {
    pub t: f64,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum StepUntilOutcome {
    InternalStep,
    RootFound { t_root: f64 },
    StopReached,
    Finished,
}

pub trait SimulationBackend {
    type Error;

    fn init(&mut self) -> Result<(), Self::Error>;
    fn step_until(&mut self, stop_time: f64) -> Result<StepUntilOutcome, Self::Error>;
    fn read_state(&self) -> BackendState;
    fn apply_event_updates(&mut self, event_time: f64) -> Result<(), Self::Error>;
    fn apply_event_actions(&mut self, event_time: f64) -> Result<(), Self::Error>;
}

#[cfg(test)]
mod tests {
    use super::{SimOptions, SimPacingMode, SimSolverMode};

    #[test]
    fn solver_mode_request_parsing_defaults_blank_input_to_auto() {
        assert_eq!(
            SimSolverMode::parse_request(None),
            (SimSolverMode::Auto, "auto".to_string())
        );
        assert_eq!(
            SimSolverMode::parse_request(Some("   ")),
            (SimSolverMode::Auto, "auto".to_string())
        );
    }

    #[test]
    fn solver_mode_request_parsing_preserves_trimmed_label_and_maps_mode() {
        assert_eq!(
            SimSolverMode::parse_request(Some("  dopri5 ")),
            (SimSolverMode::RkLike, "dopri5".to_string())
        );
        assert_eq!(
            SimSolverMode::parse_request(Some("IDA")),
            (SimSolverMode::Bdf, "IDA".to_string())
        );
    }

    #[test]
    fn simulation_options_default_to_batch_pacing() {
        assert_eq!(
            SimOptions::default().pacing_mode,
            SimPacingMode::AsFastAsPossible
        );
    }

    #[test]
    fn interactive_pacing_defaults_to_lockstep_only_for_external_coupling() {
        assert_eq!(
            SimPacingMode::resolve_interactive(None, true),
            SimPacingMode::Lockstep
        );
        assert_eq!(
            SimPacingMode::resolve_interactive(None, false),
            SimPacingMode::Realtime
        );
        assert_eq!(
            SimPacingMode::resolve_interactive(Some(SimPacingMode::AsFastAsPossible), true),
            SimPacingMode::AsFastAsPossible
        );
    }
}
