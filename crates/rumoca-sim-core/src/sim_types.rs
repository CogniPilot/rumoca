use crate::runtime::timeout::TimeoutExceeded;

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
        }
    }
}

#[derive(Debug, Clone)]
pub struct SimResult {
    pub times: Vec<f64>,
    pub names: Vec<String>,
    pub data: Vec<Vec<f64>>,
    pub n_states: usize,
    pub variable_meta: Vec<SimVariableMeta>,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SolverStartupProfile {
    Default,
    RobustTinyStep,
}

#[derive(Debug, Clone)]
pub struct OutputBuffers {
    pub times: Vec<f64>,
    pub data: Vec<Vec<f64>>,
    pub n_total: usize,
    pub runtime_names: Vec<String>,
    pub runtime_data: Vec<Vec<f64>>,
}

impl OutputBuffers {
    pub fn new(n_total: usize, capacity: usize) -> Self {
        Self {
            times: Vec::with_capacity(capacity),
            data: (0..n_total).map(|_| Vec::with_capacity(capacity)).collect(),
            n_total,
            runtime_names: Vec::new(),
            runtime_data: Vec::new(),
        }
    }

    pub fn record(&mut self, t: f64, y: &[f64]) {
        self.times.push(t);
        for (var_idx, val) in y[..self.n_total].iter().enumerate() {
            self.data[var_idx].push(*val);
        }
    }

    pub fn set_runtime_channels(&mut self, names: Vec<String>, capacity: usize) {
        self.runtime_names = names;
        self.runtime_data = self
            .runtime_names
            .iter()
            .map(|_| Vec::with_capacity(capacity))
            .collect();
    }

    pub fn record_runtime_values(&mut self, values: &[f64]) {
        if self.runtime_data.is_empty() {
            return;
        }
        for (idx, series) in self.runtime_data.iter_mut().enumerate() {
            series.push(values.get(idx).copied().unwrap_or(0.0));
        }
    }

    pub fn overwrite_runtime_values_at_time(&mut self, t: f64, values: &[f64]) -> bool {
        if self.runtime_data.is_empty() || self.times.is_empty() {
            return false;
        }
        let tol = 1.0e-9 * (1.0 + t.abs());
        let Some((row_idx, _)) = self
            .times
            .iter()
            .enumerate()
            .rev()
            .find(|(_, sample_t)| (**sample_t - t).abs() <= tol)
        else {
            return false;
        };
        for (idx, series) in self.runtime_data.iter_mut().enumerate() {
            if let Some(slot) = series.get_mut(row_idx) {
                *slot = values.get(idx).copied().unwrap_or(0.0);
            }
        }
        true
    }
}

#[derive(Debug, thiserror::Error)]
pub enum SimError {
    #[error("empty system: no equations to simulate")]
    EmptySystem,

    #[error(
        "equation/variable mismatch: {n_equations} equations but \
         {n_states} states + {n_algebraics} algebraics = {} unknowns",
        n_states + n_algebraics
    )]
    EquationMismatch {
        n_equations: usize,
        n_states: usize,
        n_algebraics: usize,
    },

    #[error(
        "no ODE equation found for state variable '{0}': \
         every state needs an equation containing der({0})"
    )]
    MissingStateEquation(String),

    #[error("solver error: {0}")]
    SolverError(String),

    #[error("unsupported function '{name}': {reason}")]
    UnsupportedFunction { name: String, reason: String },

    #[error("compiled evaluator build failed: {0}")]
    CompiledEval(String),

    #[error("timeout after {seconds:.3}s")]
    Timeout { seconds: f64 },
}

impl From<TimeoutExceeded> for SimError {
    fn from(value: TimeoutExceeded) -> Self {
        Self::Timeout {
            seconds: value.seconds,
        }
    }
}
