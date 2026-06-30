#[derive(Debug, Clone, Copy, Default)]
pub struct BuildSimulationTimings {
    pub ir_solve_structural_dae_seconds: f64,
    pub ir_solve_lower_seconds: f64,
    pub ir_solve_seconds: f64,
    pub override_apply_seconds: f64,
    pub backend_build_seconds: f64,
}

impl BuildSimulationTimings {
    pub fn accounted_seconds(self) -> f64 {
        self.ir_solve_seconds + self.override_apply_seconds + self.backend_build_seconds
    }
}
