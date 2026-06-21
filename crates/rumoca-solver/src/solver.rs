//! Backend-neutral solver contracts shared by simulation backends.

use crate::RuntimeEventBoundaryHandler;
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
        // ESDIRK34 / TR-BDF2 are *implicit* tableaus served by the diffsol
        // (BDF-family) path via `DiffsolMethod`, not the explicit rk45 backend
        // — so they resolve to `Bdf` here, not `RkLike`.
        let rk_like = normalized.contains("rungekutta")
            || normalized.starts_with("rk")
            || normalized.contains("dopri")
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

/// Which diffsol integrator to construct on the implicit (BDF-family) path.
///
/// `SimSolverMode` selects the solver *family* (auto / implicit-BDF /
/// explicit-RK). Within the implicit family, diffsol also ships A- and
/// L-stable singly-diagonally-implicit Runge-Kutta tableaus (ESDIRK34,
/// TR-BDF2) that suit stiff DAEs whose BDF startup struggles with sharp
/// near-discontinuities (tanh / relop transitions, radiative T^4). This
/// enum picks among them. `Bdf` is the default and leaves the existing
/// construction path byte-for-byte unchanged.
#[derive(Debug, Clone, Copy, Default, Deserialize, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum DiffsolMethod {
    /// Variable-order BDF (default). Best general-purpose stiff solver.
    #[default]
    Bdf,
    /// ESDIRK 3(4) — singly-diagonally-implicit, A- and L-stable.
    Esdirk34,
    /// TR-BDF2 — implicit, A- and L-stable, two-stage. Strong on moderately
    /// stiff problems with event-driven dynamics.
    TrBdf2,
}

impl DiffsolMethod {
    /// Map a user-facing solver name (case-insensitive, dashes / underscores
    /// ignored) to a specific implicit tableau. Returns `None` for names that
    /// are not implicit RK tableaus (callers keep the BDF default).
    pub fn from_external_name(name: &str) -> Option<Self> {
        let n = name
            .trim()
            .to_ascii_lowercase()
            .replace(['-', '_', ' '], "");
        if n.starts_with("esdirk") {
            Some(Self::Esdirk34)
        } else if n.starts_with("trbdf2") {
            Some(Self::TrBdf2)
        } else {
            None
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
    /// Implicit tableau to use on the diffsol (BDF-family) path. Ignored when
    /// `solver_mode` resolves to the explicit rk45 backend. Defaults to `Bdf`.
    pub diffsol_method: DiffsolMethod,
    pub pacing_mode: SimPacingMode,
    /// Tunable parameter overrides applied after lowering, keyed by scalar
    /// parameter name (e.g. `"k"`, `"gear.ratio"`). Empty by default — a plain
    /// simulation uses the model's declared values. Only tunable parameters with
    /// a runtime slot may be overridden; structural/folded/depended-upon names
    /// are rejected so an override is never silently dropped.
    pub param_overrides: Vec<(String, f64)>,
    /// State start-value overrides applied after lowering, keyed by scalar state
    /// name. These seed the initialization solve.
    pub start_overrides: Vec<(String, f64)>,
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
            diffsol_method: DiffsolMethod::Bdf,
            pacing_mode: SimPacingMode::AsFastAsPossible,
            param_overrides: Vec::new(),
            start_overrides: Vec::new(),
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

pub trait SimulationBackend: RuntimeEventBoundaryHandler {
    fn init(&mut self) -> Result<(), Self::Error>;
    fn step_until(&mut self, stop_time: f64) -> Result<StepUntilOutcome, Self::Error>;
    fn read_state(&self) -> BackendState;
}

#[cfg(test)]
mod tests {
    use super::{DiffsolMethod, SimOptions, SimPacingMode, SimSolverMode};

    #[test]
    fn implicit_tableau_names_resolve_to_sdirk_method_not_explicit_rk() {
        // ESDIRK34 / TR-BDF2 are implicit tableaus on the diffsol path: they
        // must select a `DiffsolMethod`, and must NOT be misrouted to the
        // explicit rk45 backend (`RkLike`).
        for name in ["esdirk34", "ESDIRK-34", "es_dirk34"] {
            assert_eq!(
                DiffsolMethod::from_external_name(name),
                Some(DiffsolMethod::Esdirk34)
            );
            assert_eq!(SimSolverMode::from_external_name(name), SimSolverMode::Bdf);
        }
        for name in ["trbdf2", "TR-BDF2", "tr_bdf2"] {
            assert_eq!(
                DiffsolMethod::from_external_name(name),
                Some(DiffsolMethod::TrBdf2)
            );
            assert_eq!(SimSolverMode::from_external_name(name), SimSolverMode::Bdf);
        }
        // Explicit / unknown names keep the BDF default (None) and route as before.
        assert_eq!(DiffsolMethod::from_external_name("dopri5"), None);
        assert_eq!(
            SimSolverMode::from_external_name("dopri5"),
            SimSolverMode::RkLike
        );
        assert_eq!(DiffsolMethod::from_external_name("bdf"), None);
    }

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
