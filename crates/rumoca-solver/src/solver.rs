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

/// Which execution strategy the runtime may use for compiler-issued Solve
/// programs.
///
/// This is the inspectable, typed replacement for the ad-hoc host escape hatch
/// that used to select the interpreter. The backend differential oracle needs
/// to pin one side of the comparison, so the selection must stay part of the
/// request rather than an ambient process setting.
///
/// A third value, `NativeRequired` — fail instead of silently falling back to
/// the interpreter when compiled execution is unavailable — is reserved for the
/// slice that threads a typed unavailability error out of backend
/// construction; it is deliberately not offered until it can be honored.
#[derive(Debug, Clone, Copy, Default, Deserialize, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum SimExecutionPolicy {
    /// Use compiled native execution when the target and model support it,
    /// otherwise evaluate through the Solve-IR interpreter.
    #[default]
    Auto,
    /// Always evaluate through the Solve-IR interpreter, even where compiled
    /// execution is available. This is the reference side of the backend
    /// differential oracle.
    Interpreter,
}

impl SimExecutionPolicy {
    #[must_use]
    pub const fn label(self) -> &'static str {
        match self {
            Self::Auto => "auto",
            Self::Interpreter => "interpreter",
        }
    }

    /// True when compiled native execution may be used.
    #[must_use]
    pub const fn allows_native(self) -> bool {
        matches!(self, Self::Auto)
    }
}

/// The execution engine a simulation selected for its Solve programs.
#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum SimExecutionEngine {
    /// The Solve-IR interpreter.
    Interpreter,
    /// Cranelift-compiled native programs.
    Cranelift,
    /// Portable WASM programs.
    WasmProgram,
}

/// Why compiled execution was refused for a model the policy allowed.
#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum SimNativeRefusal {
    /// The model has no continuous states; neither host instantiates an
    /// integrator component, so compiled code would be discarded unused.
    NoContinuousStates,
    /// The target's compiled profile declines external tables.
    ExternalTables,
}

impl SimNativeRefusal {
    /// Human-readable reason.
    #[must_use]
    pub const fn describe(self) -> &'static str {
        match self {
            Self::NoContinuousStates => "the model has no continuous states",
            Self::ExternalTables => "the compiled profile declines external tables",
        }
    }
}

/// One compile request the selected backend declined. The program it names runs
/// in the interpreter instead; the receipt lists it so the fallback is never
/// silent.
#[derive(Debug, Clone, Deserialize, PartialEq, Eq, Serialize)]
pub struct SimNativeDecline {
    /// The kind of program the backend was asked to compile.
    pub program: String,
    /// The backend's stated reason.
    pub reason: String,
    /// How many requests of that kind were declined for that reason.
    pub count: usize,
}

/// The typed record of which engine a simulation selected and, when compiled
/// execution was refused for an admissible request, why.
///
/// One decision site ([`Self::admission`]) produces it for every target, so
/// the receipt and the backend actually constructed cannot disagree.
#[derive(Debug, Clone, Deserialize, PartialEq, Eq, Serialize)]
pub struct SimExecutionReceipt {
    pub engine: SimExecutionEngine,
    /// Absent when compiled execution was selected or the policy pinned the
    /// interpreter.
    pub refusal: Option<SimNativeRefusal>,
    /// Programs the selected backend declined to compile, which run in the
    /// interpreter. Compilation happens lazily as the session runs, so this
    /// list is read at the time the receipt is requested.
    pub declined: Vec<SimNativeDecline>,
}

impl SimExecutionReceipt {
    /// Decide admission of `compiled` execution for a model with
    /// `state_count` continuous states and `external_tables_declined` external
    /// tables the target's profile cannot execute (zero when it can).
    #[must_use]
    pub const fn admission(
        policy: SimExecutionPolicy,
        state_count: usize,
        external_tables_declined: usize,
        compiled: SimExecutionEngine,
    ) -> Self {
        let refusal = if !policy.allows_native() {
            return Self::interpreter(None);
        } else if state_count == 0 {
            SimNativeRefusal::NoContinuousStates
        } else if external_tables_declined != 0 {
            SimNativeRefusal::ExternalTables
        } else {
            return Self {
                engine: compiled,
                refusal: None,
                declined: Vec::new(),
            };
        };
        Self::interpreter(Some(refusal))
    }

    const fn interpreter(refusal: Option<SimNativeRefusal>) -> Self {
        Self {
            engine: SimExecutionEngine::Interpreter,
            refusal,
            declined: Vec::new(),
        }
    }

    /// The receipt with the backend's declined compile requests recorded.
    #[must_use]
    pub fn with_declined(mut self, declined: Vec<SimNativeDecline>) -> Self {
        self.declined = declined;
        self
    }

    /// True when compiled execution was selected.
    #[must_use]
    pub const fn is_compiled(&self) -> bool {
        !matches!(self.engine, SimExecutionEngine::Interpreter)
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
    /// Host-provided input values at initialization, keyed by scalar input name.
    /// Missing inputs still require a checked binding. Reset restores these
    /// values before solving the initialization equations again.
    pub initial_inputs: Vec<(String, f64)>,
    /// Which execution strategy the runtime may use for compiler-issued Solve
    /// programs. Part of the request so a run's interpreter/native selection is
    /// reproducible and inspectable rather than ambient.
    pub execution_policy: SimExecutionPolicy,
    /// Memory the adjoint sensitivity may spend storing the forward trajectory,
    /// in bytes; a run that needs more is refused with a typed error (SOLVE-C76).
    pub checkpoint_budget_bytes: u64,
}

/// One gibibyte: the default memory budget of a stored forward trajectory.
pub const DEFAULT_CHECKPOINT_BUDGET_BYTES: u64 = 1 << 30;

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
            initial_inputs: Vec::new(),
            execution_policy: SimExecutionPolicy::Auto,
            checkpoint_budget_bytes: DEFAULT_CHECKPOINT_BUDGET_BYTES,
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
    /// The source scalar a generated reduced-selection state scalar equals.
    pub state_coordinate: Option<rumoca_ir_solve::SolveStateCoordinate>,
    /// The phasor a visible angle or power-factor scalar is a function of.
    pub phasor: Option<rumoca_ir_solve::SolvePhasor>,
}

#[derive(Debug, Clone)]
pub struct SimTermination {
    pub time: f64,
    pub message: String,
}

/// One non-aborting diagnostic a simulation run reports (SPEC_0008 `WX0xx`).
#[derive(Debug, Clone, PartialEq)]
pub struct SimDiagnostic {
    /// Stable mnemonic of the diagnostic, such as `WX001`.
    pub code: &'static str,
    /// Simulation time at which the condition was first observed.
    pub time: f64,
    /// The message the model supplies, rendered at that time.
    pub message: String,
    /// Source span and origin of the reporting site.
    pub span: rumoca_core::Span,
    pub origin: String,
}

/// SPEC_0008 `WX001`: a warning-level assertion (MLS §8.3.7) was violated.
pub const WARNING_ASSERTION_CODE: &str = "WX001";

/// SPEC_0008 `WX002`: the model printed a message to the terminal (MLS 3.7
/// §12.9, `Modelica.Utilities.Streams.print`).
pub const MODEL_MESSAGE_CODE: &str = "WX002";

#[derive(Debug, Clone)]
pub struct SimResult {
    pub times: Vec<f64>,
    pub names: Vec<String>,
    pub data: Vec<Vec<f64>>,
    pub n_states: usize,
    pub variable_meta: Vec<SimVariableMeta>,
    pub termination: Option<SimTermination>,
    /// Non-aborting diagnostics in order of first occurrence.
    pub diagnostics: Vec<SimDiagnostic>,
}

#[cfg(test)]
mod tests {
    use super::{
        DiffsolMethod, SimExecutionEngine, SimExecutionPolicy, SimExecutionReceipt,
        SimNativeRefusal, SimOptions, SimPacingMode, SimSolverMode,
    };

    #[test]
    fn admission_names_the_engine_and_the_refusal() {
        use SimExecutionEngine::{Cranelift, Interpreter, WasmProgram};
        let admit = |policy, states, tables, engine| {
            SimExecutionReceipt::admission(policy, states, tables, engine)
        };
        let compiled = admit(SimExecutionPolicy::Auto, 2, 0, Cranelift);
        assert_eq!((compiled.engine, compiled.refusal), (Cranelift, None));
        assert!(compiled.is_compiled());
        let wasm = admit(SimExecutionPolicy::Auto, 1, 0, WasmProgram);
        assert_eq!(wasm.engine, WasmProgram);
        let pinned = admit(SimExecutionPolicy::Interpreter, 2, 3, Cranelift);
        assert_eq!((pinned.engine, pinned.refusal), (Interpreter, None));
        let stateless = admit(SimExecutionPolicy::Auto, 0, 0, Cranelift);
        assert_eq!(
            stateless.refusal,
            Some(SimNativeRefusal::NoContinuousStates)
        );
        assert!(!stateless.is_compiled());
        let tables = admit(SimExecutionPolicy::Auto, 1, 1, WasmProgram);
        assert_eq!(tables.refusal, Some(SimNativeRefusal::ExternalTables));
        assert_eq!(tables.engine, Interpreter);
        for refusal in [
            SimNativeRefusal::NoContinuousStates,
            SimNativeRefusal::ExternalTables,
        ] {
            assert!(!refusal.describe().is_empty());
        }
    }

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
