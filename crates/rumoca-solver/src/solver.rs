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
        // The ESDIRK34 / TR-BDF2 names are *implicit* tableaus, not the explicit
        // rk45 backend, so they land in `Bdf` here rather than `RkLike`.
        // `DiffsolMethod::from_external_name` reports that they are temporarily
        // unavailable; this classifier only picks the solver family.
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
/// explicit-RK). Within the implicit family, BDF is the only integrator, so
/// there is no name-to-tableau mapping to perform: a caller that has resolved a
/// solver name through [`rumoca_core::canonical_solver_name`] and landed in the
/// implicit family gets [`Self::Bdf`].
///
/// The enum is kept single-variant rather than folded away so the integrator a
/// run used stays an explicit part of the recorded request.
#[derive(Debug, Clone, Copy, Default, Deserialize, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum DiffsolMethod {
    /// Variable-order BDF. The only diffsol integrator.
    #[default]
    Bdf,
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
    pub fn default_for_coupling(has_external_coupling: bool) -> Self {
        if has_external_coupling {
            Self::Lockstep
        } else {
            Self::Realtime
        }
    }

    #[must_use]
    pub fn resolve_schedule(explicit: Option<Self>, has_external_coupling: bool) -> Self {
        explicit.unwrap_or_else(|| Self::default_for_coupling(has_external_coupling))
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
    /// Requested output interval. For zero-continuous-state models, a finite
    /// positive value is also the maximum interval between relation-surface
    /// observations. This makes root detection resolution-bounded; it does not
    /// mathematically exclude multiple unobserved roots inside one interval.
    pub dt: Option<f64>,
    pub scalarize: bool,
    pub max_wall_seconds: Option<f64>,
    pub solver_mode: SimSolverMode,
    /// Integrator to use on the diffsol (BDF-family) path. Ignored when
    /// `solver_mode` resolves to the explicit rk45 backend. BDF is the only
    /// remaining choice; the field is kept so the resolved integrator stays an
    /// explicit part of the request rather than an implicit assumption.
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

/// An integrator that advances a model in time.
///
/// SPEC_0038 §Internal Solver Boundary: a backend integrates; it does not own
/// event semantics. Applying an event boundary is a separate capability
/// ([`RuntimeEventBoundaryHandler`]), required only by hosts that drive the
/// shared orchestration loop rather than an ME component's own event mode.
pub trait SimulationBackend {
    type Error;

    fn init(&mut self) -> Result<(), Self::Error>;
    fn step_until(&mut self, stop_time: f64) -> Result<StepUntilOutcome, Self::Error>;
    fn read_state(&self) -> BackendState;
}

#[cfg(test)]
mod tests {
    use super::{DiffsolMethod, SimOptions, SimPacingMode, SimSolverMode};

    /// The implicit family carries exactly one integrator, so there is nothing
    /// left for a name to select inside it. Any name-level rejection now belongs
    /// to `rumoca_core::canonical_solver_name`, which owns the whole valid set.
    #[test]
    fn the_implicit_family_offers_exactly_one_integrator() {
        assert_eq!(DiffsolMethod::default(), DiffsolMethod::Bdf);
        for name in ["bdf", ""] {
            assert_eq!(SimSolverMode::from_external_name(name), SimSolverMode::Bdf);
        }
        assert_eq!(
            SimSolverMode::from_external_name("auto"),
            SimSolverMode::Auto
        );
        assert_eq!(
            SimSolverMode::from_external_name("dopri5"),
            SimSolverMode::RkLike
        );
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
    fn scheduled_pacing_defaults_to_lockstep_only_for_external_coupling() {
        assert_eq!(
            SimPacingMode::resolve_schedule(None, true),
            SimPacingMode::Lockstep
        );
        assert_eq!(
            SimPacingMode::resolve_schedule(None, false),
            SimPacingMode::Realtime
        );
        assert_eq!(
            SimPacingMode::resolve_schedule(Some(SimPacingMode::AsFastAsPossible), true),
            SimPacingMode::AsFastAsPossible
        );
    }
}
