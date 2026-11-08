use rumoca_compile::compile::CompilationResult;
use rumoca_sim::SimOptions;
use serde::{Deserialize, Serialize};

/// Annotation data used identically by source and precompiled sessions.
#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct SimulationExperiment {
    experiment_start_time: Option<f64>,
    experiment_stop_time: Option<f64>,
    experiment_tolerance: Option<f64>,
    experiment_interval: Option<f64>,
    experiment_solver: Option<String>,
}

impl From<&CompilationResult> for SimulationExperiment {
    fn from(result: &CompilationResult) -> Self {
        Self {
            experiment_start_time: result.experiment_start_time,
            experiment_stop_time: result.experiment_stop_time,
            experiment_tolerance: result.experiment_tolerance,
            experiment_interval: result.experiment_interval,
            experiment_solver: result.experiment_solver.clone(),
        }
    }
}

impl SimulationExperiment {
    pub(crate) fn simulation_options(
        &self,
        t_end: f64,
        dt: f64,
        solver: &str,
    ) -> (SimOptions, String) {
        let solver_request = if solver.trim().is_empty() {
            self.experiment_solver.as_deref().unwrap_or(solver)
        } else {
            solver
        };
        let (solver_mode, solver_label) = super::parse_wasm_solver_request(solver_request);
        let defaults = SimOptions::default();
        let t_start = self
            .experiment_start_time
            .filter(|value| value.is_finite())
            .unwrap_or(defaults.t_start);
        let mut resolved_t_end = if t_end > 0.0 {
            t_end
        } else {
            self.experiment_stop_time
                .filter(|value| value.is_finite())
                .unwrap_or(defaults.t_end)
        };
        if resolved_t_end <= t_start {
            resolved_t_end = t_start + 1.0;
        }
        let tolerance = self
            .experiment_tolerance
            .filter(|value| value.is_finite() && *value > 0.0);
        let dt_opt = if dt > 0.0 {
            Some(dt)
        } else {
            self.experiment_interval
                .filter(|value| value.is_finite() && *value > 0.0)
        };
        (
            SimOptions {
                t_start,
                t_end: resolved_t_end,
                rtol: tolerance.unwrap_or(defaults.rtol),
                atol: tolerance.unwrap_or(defaults.atol),
                dt: dt_opt,
                solver_mode,
                ..defaults
            },
            solver_label,
        )
    }
}
