use std::time::Instant;

#[cfg(feature = "scheduled-sim")]
use indexmap::IndexMap;
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use crate::BuildSimulationTimings;
#[cfg(feature = "scheduled-sim")]
use crate::SimulationSessionApi;
use crate::solve_lowering::{
    SimulationDiagnosticError, lower_dae_for_simulation_with_stage_timing_and_param_overrides,
    tunable_param_overrides,
};

pub(crate) use rumoca_solver_diffsol::session::SessionState;
pub use rumoca_solver_diffsol::{SimError, SimFailureStage};

// Native-backend composition for the BDF host goes through the ONE shared
// sim-side admission gate,
// [`crate::native_execution::admitted_native_execution_backend`], exactly as
// on the rk-like path: `rumoca-solver-diffsol` only ever receives the opaque
// handle and can neither construct nor unwrap a backend of its own, and the
// interpreter-policy / zero-state withholding rules cannot drift between the
// two concrete paths.

pub struct PreparedSimulation {
    inner: rumoca_solver_diffsol::PreparedSimulation,
    model: solve::SolveModel,
}

impl PreparedSimulation {
    pub fn backend(&self) -> rumoca_solver::SimBackend {
        self.inner.backend()
    }

    pub fn run(&self) -> Result<rumoca_solver::SimResult, SimError> {
        self.inner.run()
    }

    pub fn check_initialization(&self) -> Result<(), SimError> {
        self.inner.check_initialization()
    }

    pub fn model(&self) -> &solve::SolveModel {
        &self.model
    }

    pub fn set_parameter_value(&mut self, _name: &str, _value: f64) -> Result<(), SimError> {
        Err(SimError::SolverError(
            "parameter overrides are not yet supported after Solve IR lowering".to_string(),
        ))
    }

    pub fn set_parameter_values(&mut self, _name: &str, _values: &[f64]) -> Result<(), SimError> {
        Err(SimError::SolverError(
            "parameter overrides are not yet supported after Solve IR lowering".to_string(),
        ))
    }

    pub fn clear_parameter_overrides(&mut self) {}
}

pub fn build_simulation(
    dae_model: &dae::Dae,
    opts: &rumoca_solver::SimOptions,
) -> Result<PreparedSimulation, SimError> {
    build_simulation_with_stage_timing(dae_model, opts, |_| {}).map(|(prepared, _)| prepared)
}

pub fn build_simulation_with_stage_timing(
    dae_model: &dae::Dae,
    opts: &rumoca_solver::SimOptions,
    begin_stage: impl FnMut(&'static str),
) -> Result<(PreparedSimulation, BuildSimulationTimings), SimError> {
    build_simulation_with_stage_timing_and_solve_model(dae_model, opts, begin_stage, |_| {})
}

pub fn build_simulation_with_stage_timing_and_solve_model(
    dae_model: &dae::Dae,
    opts: &rumoca_solver::SimOptions,
    mut begin_stage: impl FnMut(&'static str),
    mut observe_solve_model: impl FnMut(&solve::SolveModel),
) -> Result<(PreparedSimulation, BuildSimulationTimings), SimError> {
    let param_overrides = tunable_param_overrides(dae_model, opts).map_err(diagnostic_sim_error)?;
    let (mut solve_model, solve_timings) =
        lower_dae_for_simulation_with_stage_timing_and_param_overrides(
            dae_model,
            opts,
            &param_overrides,
            &mut begin_stage,
        )
        .map_err(diagnostic_sim_error)?;
    begin_stage("sim_overrides");
    let override_apply_start = Instant::now();
    crate::solve_lowering::apply_simulation_overrides(&mut solve_model, dae_model, opts)
        .map_err(diagnostic_sim_error)?;
    let override_apply_seconds = override_apply_start.elapsed().as_secs_f64();
    observe_solve_model(&solve_model);
    begin_stage("sim_build");
    let backend_build_start = Instant::now();
    let execution_backend =
        crate::native_execution::admitted_native_execution_backend(opts, &solve_model);
    let inner = rumoca_solver_diffsol::build_simulation_with_execution_backend(
        &solve_model,
        opts,
        execution_backend,
    )?;
    let backend_build_seconds = backend_build_start.elapsed().as_secs_f64();
    Ok((
        PreparedSimulation {
            inner,
            model: solve_model,
        },
        BuildSimulationTimings {
            ir_solve_structural_dae_seconds: solve_timings.ir_solve_structural_dae_seconds,
            ir_solve_lower_seconds: solve_timings.ir_solve_lower_seconds,
            ir_solve_seconds: solve_timings.ir_solve_seconds,
            override_apply_seconds,
            backend_build_seconds,
        },
    ))
}

pub fn run_prepared_simulation(
    prepared: &PreparedSimulation,
) -> Result<rumoca_solver::SimResult, SimError> {
    prepared.run()
}

pub fn check_prepared_initialization(prepared: &PreparedSimulation) -> Result<(), SimError> {
    prepared.check_initialization()
}

pub fn check_initialization(
    dae_model: &dae::Dae,
    opts: &rumoca_solver::SimOptions,
) -> Result<(), SimError> {
    let solve_model = crate::solve_lowering::lower_for_simulation_with_overrides(dae_model, opts)
        .map_err(diagnostic_sim_error)?;
    let execution_backend =
        crate::native_execution::admitted_native_execution_backend(opts, &solve_model);
    rumoca_solver_diffsol::check_initialization_with_execution_backend(
        &solve_model,
        opts,
        execution_backend,
    )
}

pub fn simulate(
    dae_model: &dae::Dae,
    opts: &rumoca_solver::SimOptions,
) -> Result<rumoca_solver::SimResult, SimError> {
    let solve_model = crate::solve_lowering::lower_for_simulation_with_overrides(dae_model, opts)
        .map_err(diagnostic_sim_error)?;
    let execution_backend =
        crate::native_execution::admitted_native_execution_backend(opts, &solve_model);
    rumoca_solver_diffsol::simulate_with_execution_backend(&solve_model, opts, execution_backend)
}

pub use simulate as simulate_dae;

pub(crate) fn simulate_with_diagnostics(
    dae_model: &dae::Dae,
    opts: &rumoca_solver::SimOptions,
) -> Result<rumoca_solver::SimResult, SimulationDiagnosticError> {
    let solve_model = crate::solve_lowering::lower_for_simulation_with_overrides(dae_model, opts)?;
    let execution_backend =
        crate::native_execution::admitted_native_execution_backend(opts, &solve_model);
    rumoca_solver_diffsol::simulate_with_execution_backend(&solve_model, opts, execution_backend)
        .map_err(|err| SimulationDiagnosticError::Solver(err.to_string()))
}

pub(crate) struct SimulationSession {
    inner: rumoca_solver_diffsol::session::SimulationSession,
}

impl SimulationSession {
    /// Build directly from an already-lowered, override-applied solve model, so
    /// callers that lowered once (e.g. auto solver dispatch that first
    /// probes for a pure-discrete model) do not lower the model a second time.
    pub(crate) fn from_solve_model(
        solve_model: solve::SolveModel,
        opts: rumoca_solver::SimOptions,
    ) -> Result<Self, SimulationDiagnosticError> {
        let execution_backend =
            crate::native_execution::admitted_native_execution_backend(&opts, &solve_model);
        let inner = rumoca_solver_diffsol::session::SimulationSession::new_with_execution_backend(
            &solve_model,
            opts,
            execution_backend,
        )
        .map_err(|err| SimulationDiagnosticError::Solver(err.to_string()))?;
        Ok(Self { inner })
    }

    pub(crate) fn set_input(&mut self, name: &str, value: f64) -> Result<(), SimError> {
        self.inner.set_input(name, value)
    }

    pub(crate) fn advance_to(&mut self, target_time: f64) -> Result<(), SimError> {
        self.inner.advance_to(target_time)
    }

    pub(crate) fn ensure_end_time(&mut self, target_time: f64) {
        self.inner.ensure_end_time(target_time);
    }

    pub(crate) fn step(&mut self, dt: f64) -> Result<(), SimError> {
        if dt <= 0.0 {
            return Ok(());
        }
        self.advance_to(self.time() + dt)
    }

    pub(crate) fn reset(&mut self, t_start: f64) -> Result<(), SimError> {
        self.inner.reset(t_start)
    }

    pub(crate) fn time(&self) -> f64 {
        self.inner.time()
    }

    pub(crate) fn get(&self, name: &str) -> Result<Option<f64>, SimError> {
        self.inner.get(name)
    }

    pub(crate) fn state(&self) -> Result<SessionState, SimError> {
        self.inner.state()
    }

    #[cfg(feature = "scheduled-sim")]
    pub(crate) fn values_for(&self, names: &[String]) -> Result<IndexMap<String, f64>, SimError> {
        self.inner.values_for(names)
    }

    pub(crate) fn input_names(&self) -> &[String] {
        self.inner.input_names()
    }

    pub(crate) fn variable_names(&self) -> &[String] {
        self.inner.variable_names()
    }

    #[cfg(feature = "scheduled-sim")]
    pub(crate) fn max_schedule_advance_dt(&self) -> Option<f64> {
        self.inner.max_schedule_advance_dt()
    }
}

/// Preserve the originating diagnostic code when adapting to the backend's
/// string-carrying error.
/// diagnostic, which also carries the runtime `EX0xx` codes (notably `EX003`
/// for a rejected parameter/start override — otherwise unreachable downstream,
/// because a re-derivation from the stringified `SimError` can only ever
/// produce the `EX001`/`EX002` fallbacks).
fn diagnostic_sim_error(err: SimulationDiagnosticError) -> SimError {
    // Carry the stage as typed data too. The `[CODE] ` tag is what the CLI
    // renders and what code-recovery reads back; the stage is what failure
    // classification consumes, so it must not have to re-parse that tag.
    let stage = diagnostic_failure_stage(&err);
    SimError::SolveIr(format!("[{}] {err}", err.diagnostic_code())).at_stage(stage)
}

/// The simulation stage a lowering/preparation diagnostic belongs to, read off
/// the error variant rather than its rendered text.
fn diagnostic_failure_stage(err: &SimulationDiagnosticError) -> SimFailureStage {
    match err {
        SimulationDiagnosticError::SolveLowering(_) if err.is_structural() => {
            SimFailureStage::StructuralAnalysis
        }
        SimulationDiagnosticError::SolveLowering(_)
        | SimulationDiagnosticError::InvalidOverride { .. } => SimFailureStage::SolveLowering,
        SimulationDiagnosticError::RuntimePreparation { .. } => SimFailureStage::BackendBuild,
        SimulationDiagnosticError::Solver(_) => SimFailureStage::Integration,
    }
}

#[cfg(feature = "scheduled-sim")]
impl SimulationSessionApi for SimulationSession {
    type Error = SimError;

    fn reset(&mut self, t_start: f64) -> Result<(), Self::Error> {
        Self::reset(self, t_start)
    }

    fn set_input(&mut self, name: &str, value: f64) -> Result<(), Self::Error> {
        Self::set_input(self, name, value)
    }

    fn ensure_end_time(&mut self, target_time: f64) {
        Self::ensure_end_time(self, target_time);
    }

    fn advance_to(&mut self, target_time: f64) -> Result<(), Self::Error> {
        Self::advance_to(self, target_time)
    }

    fn time(&self) -> f64 {
        Self::time(self)
    }

    fn get(&self, name: &str) -> Result<Option<f64>, Self::Error> {
        Self::get(self, name)
    }

    fn max_schedule_advance_dt(&self) -> Option<f64> {
        Self::max_schedule_advance_dt(self)
    }
}

/// Discriminator tests for the diffsol native-composition evacuation
/// (SPEC_0041 §4, bound by SPEC_0029 §12).
///
/// These live in `rumoca-sim` deliberately: this crate legitimately owns the
/// `rumoca-exec-cranelift` dependency and the target/runtime composition, so a
/// counting wrapper around the real compiled backend is test fixture here and
/// would be contraband anywhere in `rumoca-solver-diffsol`.
#[cfg(all(test, not(target_arch = "wasm32")))]
mod native_policy_tests {
    use std::cell::Cell;
    use std::rc::Rc;

    use rumoca_compile::compile::{Session, SessionConfig};
    use rumoca_ir_solve as solve;
    use rumoca_solver::{
        CompiledSolveAssignmentSchedule, CompiledSolveEventTransaction, CompiledSolveExpression,
        CompiledSolveJacobianExpression, SimExecutionPolicy, SimOptions, SolveExecutionBackend,
        fmi_me::MeExecutionBackend,
    };

    use crate::native_execution::admitted_native_execution_backend;

    /// Attempted / succeeded / failed accounting for one native call class.
    ///
    /// The split matters because the runtime treats a compiled-call failure as
    /// permission to fall back to the interpreter: counting *attempts* alone
    /// would let a backend that errors on every call masquerade as native
    /// execution. Success is recorded only after the delegated call returns
    /// `Ok`.
    #[derive(Default)]
    struct CallClassCounters {
        attempted: Cell<usize>,
        succeeded: Cell<usize>,
        failed: Cell<usize>,
    }

    impl CallClassCounters {
        fn observe<T, E>(&self, result: &Result<T, E>) {
            self.attempted.set(self.attempted.get() + 1);
            match result {
                Ok(_) => self.succeeded.set(self.succeeded.get() + 1),
                Err(_) => self.failed.set(self.failed.get() + 1),
            }
        }
    }

    #[derive(Default)]
    struct NativeCallCounters {
        expression_compiles: Cell<usize>,
        jacobian_compiles: Cell<usize>,
        assignment_compiles: Cell<usize>,
        event_transaction_compiles: Cell<usize>,
        expression: CallClassCounters,
        jacobian: CallClassCounters,
        assignment: CallClassCounters,
        event_transaction: CallClassCounters,
    }

    impl NativeCallCounters {
        fn bump(cell: &Cell<usize>) {
            cell.set(cell.get() + 1);
        }

        fn total_succeeded(&self) -> usize {
            self.expression.succeeded.get()
                + self.jacobian.succeeded.get()
                + self.assignment.succeeded.get()
                + self.event_transaction.succeeded.get()
        }

        fn total_failed(&self) -> usize {
            self.expression.failed.get()
                + self.jacobian.failed.get()
                + self.assignment.failed.get()
                + self.event_transaction.failed.get()
        }

        /// Snapshot of the four compile counters, for exact compile
        /// cardinality pins (`[expression, jacobian, assignment,
        /// event_transaction]`).
        fn compile_counts(&self) -> [usize; 4] {
            [
                self.expression_compiles.get(),
                self.jacobian_compiles.get(),
                self.assignment_compiles.get(),
                self.event_transaction_compiles.get(),
            ]
        }

        /// Every observable interaction with the backend: compiles plus call
        /// attempts. The typed-rejection and zero-state discriminators assert
        /// this is zero — the backend must never be touched at all.
        fn total_activity(&self) -> usize {
            self.expression_compiles.get()
                + self.jacobian_compiles.get()
                + self.assignment_compiles.get()
                + self.event_transaction_compiles.get()
                + self.expression.attempted.get()
                + self.jacobian.attempted.get()
                + self.assignment.attempted.get()
                + self.event_transaction.attempted.get()
        }
    }

    /// The native-success evidence the Auto-path discriminator demands: every
    /// required call class (expression, JVP, exact assignment) executed
    /// natively at least once AND no native call of any class failed. A
    /// backend that errors on every call bumps `failed` and cannot satisfy
    /// this, however many attempts it logs — silent interpreter fallback is
    /// exposed, not absorbed.
    fn native_success_evidence_holds(counters: &NativeCallCounters) -> bool {
        counters.expression.succeeded.get() > 0
            && counters.jacobian.succeeded.get() > 0
            && counters.assignment.succeeded.get() > 0
            && counters.total_failed() == 0
    }

    /// Counts every compile and every native call while delegating to the real
    /// Cranelift backend, so the counted runs execute exactly the production
    /// composition.
    struct CountingBackend {
        inner: Rc<dyn SolveExecutionBackend>,
        counters: Rc<NativeCallCounters>,
    }

    struct CountingExpression {
        inner: Rc<dyn CompiledSolveExpression>,
        counters: Rc<NativeCallCounters>,
    }

    impl CompiledSolveExpression for CountingExpression {
        fn call(
            &self,
            y: &[f64],
            p: &[f64],
            t: f64,
            external_tables: &[rumoca_core::ExternalTableData],
            out: &mut [f64],
        ) -> Result<(), String> {
            let result = self.inner.call(y, p, t, external_tables, out);
            self.counters.expression.observe(&result);
            result
        }
    }

    struct CountingJacobian {
        inner: Rc<dyn CompiledSolveJacobianExpression>,
        counters: Rc<NativeCallCounters>,
    }

    impl CompiledSolveJacobianExpression for CountingJacobian {
        fn call(
            &self,
            y: &[f64],
            p: &[f64],
            t: f64,
            seed: &[f64],
            external_tables: &[rumoca_core::ExternalTableData],
            out: &mut [f64],
        ) -> Result<(), String> {
            let result = self.inner.call(y, p, t, seed, external_tables, out);
            self.counters.jacobian.observe(&result);
            result
        }
    }

    struct CountingAssignment {
        inner: Rc<dyn CompiledSolveAssignmentSchedule>,
        counters: Rc<NativeCallCounters>,
    }

    impl CompiledSolveAssignmentSchedule for CountingAssignment {
        fn call(
            &self,
            y: &mut [f64],
            p: &[f64],
            t: f64,
            external_tables: &[rumoca_core::ExternalTableData],
        ) -> Result<(), String> {
            let result = self.inner.call(y, p, t, external_tables);
            self.counters.assignment.observe(&result);
            result
        }
    }

    struct CountingEventTransaction {
        inner: Rc<dyn CompiledSolveEventTransaction>,
        counters: Rc<NativeCallCounters>,
    }

    impl CompiledSolveEventTransaction for CountingEventTransaction {
        fn call(&self, input: &[f64], output: &mut [f64]) -> Result<(), String> {
            let result = self.inner.call(input, output);
            self.counters.event_transaction.observe(&result);
            result
        }
    }

    impl SolveExecutionBackend for CountingBackend {
        fn compile_expression(
            &self,
            block: &solve::ScalarProgramBlock,
        ) -> Result<Rc<dyn CompiledSolveExpression>, String> {
            NativeCallCounters::bump(&self.counters.expression_compiles);
            self.inner.compile_expression(block).map(|inner| {
                Rc::new(CountingExpression {
                    inner,
                    counters: self.counters.clone(),
                }) as Rc<_>
            })
        }

        fn compile_jacobian_expression(
            &self,
            block: &solve::ScalarProgramBlock,
        ) -> Result<Rc<dyn CompiledSolveJacobianExpression>, String> {
            NativeCallCounters::bump(&self.counters.jacobian_compiles);
            self.inner.compile_jacobian_expression(block).map(|inner| {
                Rc::new(CountingJacobian {
                    inner,
                    counters: self.counters.clone(),
                }) as Rc<_>
            })
        }

        fn compile_assignment_schedule(
            &self,
            source: &solve::ComputeBlock,
            owners: &solve::ContinuousRefreshOwners,
            schedule: &solve::ExactRefreshAssignmentSchedule,
        ) -> Result<Rc<dyn CompiledSolveAssignmentSchedule>, String> {
            NativeCallCounters::bump(&self.counters.assignment_compiles);
            self.inner
                .compile_assignment_schedule(source, owners, schedule)
                .map(|inner| {
                    Rc::new(CountingAssignment {
                        inner,
                        counters: self.counters.clone(),
                    }) as Rc<_>
                })
        }

        fn compile_event_transaction(
            &self,
            program: &solve::EventTransactionProgram,
        ) -> Result<Rc<dyn CompiledSolveEventTransaction>, String> {
            NativeCallCounters::bump(&self.counters.event_transaction_compiles);
            self.inner.compile_event_transaction(program).map(|inner| {
                Rc::new(CountingEventTransaction {
                    inner,
                    counters: self.counters.clone(),
                }) as Rc<_>
            })
        }
    }

    fn lower(source: &str, model: &str, opts: &SimOptions) -> solve::SolveModel {
        let mut session = Session::new(SessionConfig::default());
        session
            .add_document("native_policy_fixture.mo", source)
            .expect("fixture parses");
        let dae = session
            .compile_model(model)
            .expect("fixture compiles through checked ToDAE")
            .dae;
        crate::solve_lowering::lower_for_simulation_with_overrides(&dae, opts)
            .expect("fixture lowers to a Solve model")
    }

    /// One state solved by a *nonlinear* initial equation (initialization
    /// Newton, whose Jacobian-vector products go through the compiled JVP),
    /// one nonlinear algebraic (`y`), and one affine algebraic (`z`, exact
    /// assignment schedule), so a single native run must exercise compiled
    /// expression, compiled JVP, and compiled exact-assignment calls.
    fn state_fixture(opts: &SimOptions) -> solve::SolveModel {
        lower(
            concat!(
                "model NativeDiscriminator\n",
                "  Real x(start = 1);\n",
                "  Real y(start = 0.5);\n",
                "  Real z;\n",
                "initial equation\n",
                "  x = 2 - 0.5 * cos(x);\n",
                "equation\n",
                "  der(x) = -y - z;\n",
                "  y + 0.1 * sin(y) = 2 * x;\n",
                "  z = 0.5 * x + cos(time);\n",
                "end NativeDiscriminator;\n",
            ),
            "NativeDiscriminator",
            opts,
        )
    }

    fn zero_state_fixture(opts: &SimOptions) -> solve::SolveModel {
        lower(
            concat!(
                "model NativeZeroState\n",
                "  Real k;\n",
                "equation\n",
                "  k = 2 + time;\n",
                "end NativeZeroState;\n",
            ),
            "NativeZeroState",
            opts,
        )
    }

    /// Pure-discrete zero-state fixture: no continuous states, one sampled
    /// counter. This is the exact shape the rk-like no-state session accepts
    /// (a purely algebraic zero-state model is rejected as `EmptySystem`
    /// there), and the shape the zero-state composition rule exists for.
    fn zero_state_discrete_fixture(opts: &SimOptions) -> solve::SolveModel {
        lower(
            concat!(
                "model NativeZeroStateDiscrete\n",
                "  discrete Real n(start = 0, fixed = true);\n",
                "equation\n",
                "  when sample(0.1, 0.1) then\n",
                "    n = pre(n) + 1;\n",
                "  end when;\n",
                "end NativeZeroStateDiscrete;\n",
            ),
            "NativeZeroStateDiscrete",
            opts,
        )
    }

    fn sim_opts(policy: SimExecutionPolicy) -> SimOptions {
        SimOptions {
            t_end: 0.5,
            dt: Some(0.1),
            max_wall_seconds: Some(30.0),
            execution_policy: policy,
            ..SimOptions::default()
        }
    }

    fn counting_handle_over(
        inner: Rc<dyn SolveExecutionBackend>,
    ) -> (
        Rc<CountingBackend>,
        Rc<NativeCallCounters>,
        MeExecutionBackend,
    ) {
        let counters = Rc::new(NativeCallCounters::default());
        let backend = Rc::new(CountingBackend {
            inner,
            counters: counters.clone(),
        });
        let handle = MeExecutionBackend::new(backend.clone() as Rc<dyn SolveExecutionBackend>);
        (backend, counters, handle)
    }

    /// Counting decorator over the REAL Cranelift backend — the production
    /// composition, observed.
    fn counting_handle(
        model: &solve::SolveModel,
    ) -> (
        Rc<CountingBackend>,
        Rc<NativeCallCounters>,
        MeExecutionBackend,
    ) {
        counting_handle_over(crate::native_execution::backend(&model.pure_calls))
    }

    /// Mutation fixture: a backend whose compiles succeed but whose every
    /// compiled call returns `Err`. The runtime's documented semantics treat a
    /// compiled-call failure as permission to fall back to the interpreter, so
    /// a run over this backend can still complete — the success/failure
    /// accounting is what must expose it.
    struct FailingCompiled;

    impl CompiledSolveExpression for FailingCompiled {
        fn call(
            &self,
            _y: &[f64],
            _p: &[f64],
            _t: f64,
            _external_tables: &[rumoca_core::ExternalTableData],
            _out: &mut [f64],
        ) -> Result<(), String> {
            Err("injected native expression failure".to_string())
        }
    }

    impl CompiledSolveJacobianExpression for FailingCompiled {
        fn call(
            &self,
            _y: &[f64],
            _p: &[f64],
            _t: f64,
            _seed: &[f64],
            _external_tables: &[rumoca_core::ExternalTableData],
            _out: &mut [f64],
        ) -> Result<(), String> {
            Err("injected native JVP failure".to_string())
        }
    }

    impl CompiledSolveAssignmentSchedule for FailingCompiled {
        fn call(
            &self,
            _y: &mut [f64],
            _p: &[f64],
            _t: f64,
            _external_tables: &[rumoca_core::ExternalTableData],
        ) -> Result<(), String> {
            Err("injected native assignment failure".to_string())
        }
    }

    impl CompiledSolveEventTransaction for FailingCompiled {
        fn call(&self, _input: &[f64], _output: &mut [f64]) -> Result<(), String> {
            Err("injected native event-transaction failure".to_string())
        }
    }

    struct FailingBackend;

    impl SolveExecutionBackend for FailingBackend {
        fn compile_expression(
            &self,
            _block: &solve::ScalarProgramBlock,
        ) -> Result<Rc<dyn CompiledSolveExpression>, String> {
            Ok(Rc::new(FailingCompiled))
        }

        fn compile_jacobian_expression(
            &self,
            _block: &solve::ScalarProgramBlock,
        ) -> Result<Rc<dyn CompiledSolveJacobianExpression>, String> {
            Ok(Rc::new(FailingCompiled))
        }

        fn compile_assignment_schedule(
            &self,
            _source: &solve::ComputeBlock,
            _owners: &solve::ContinuousRefreshOwners,
            _schedule: &solve::ExactRefreshAssignmentSchedule,
        ) -> Result<Rc<dyn CompiledSolveAssignmentSchedule>, String> {
            Ok(Rc::new(FailingCompiled))
        }

        fn compile_event_transaction(
            &self,
            _program: &solve::EventTransactionProgram,
        ) -> Result<Rc<dyn CompiledSolveEventTransaction>, String> {
            Ok(Rc::new(FailingCompiled))
        }
    }

    /// Discriminator (a): the Auto/native BDF path really executes compiled
    /// expression, JVP, and exact-assignment native calls — SUCCESSFULLY. The
    /// runtime treats a compiled-call failure as permission to interpret, so
    /// each class asserts `succeeded > 0` (recorded only after the delegated
    /// call returns `Ok`) AND `failed == 0`: a mutation that drops, ignores,
    /// or re-composes the handle zeroes the successes, and a backend that
    /// errors its way into silent interpreter fallback trips the zero-failure
    /// assertion instead of passing on attempts.
    #[test]
    fn auto_native_bdf_path_performs_counted_native_calls() {
        let opts = sim_opts(SimExecutionPolicy::Auto);
        let model = state_fixture(&opts);
        let (_backend, counters, handle) = counting_handle(&model);
        let result =
            rumoca_solver_diffsol::simulate_with_execution_backend(&model, &opts, Some(handle))
                .expect("native BDF run succeeds");
        assert!(!result.times.is_empty(), "BDF run produced no samples");
        for (class, counter) in [
            ("expression", &counters.expression),
            ("JVP", &counters.jacobian),
            ("exact-assignment", &counters.assignment),
        ] {
            assert!(
                counter.succeeded.get() > 0,
                "compiled {class} never executed natively to completion: attempted={} \
                 succeeded={} failed={}",
                counter.attempted.get(),
                counter.succeeded.get(),
                counter.failed.get()
            );
            assert_eq!(
                counter.failed.get(),
                0,
                "compiled {class} calls failed and fell back to the interpreter: attempted={} \
                 succeeded={}",
                counter.attempted.get(),
                counter.succeeded.get()
            );
        }
        assert_eq!(
            counters.event_transaction.failed.get(),
            0,
            "compiled event-transaction calls failed and fell back to the interpreter"
        );
        assert!(
            native_success_evidence_holds(&counters),
            "the native-success evidence predicate must agree with the per-class assertions"
        );
    }

    /// Discriminator (a-mutation): a backend whose every compiled call fails
    /// must NOT satisfy the native-success evidence. The runtime's current
    /// fallback semantics let the simulation complete on the interpreter (this
    /// slice deliberately does not restructure that), so the proof is in the
    /// accounting: nonzero failures, zero successes, evidence predicate false.
    #[test]
    fn failing_native_backend_cannot_satisfy_the_success_evidence() {
        let opts = sim_opts(SimExecutionPolicy::Auto);
        let model = state_fixture(&opts);
        let (_backend, counters, handle) = counting_handle_over(Rc::new(FailingBackend));
        let result =
            rumoca_solver_diffsol::simulate_with_execution_backend(&model, &opts, Some(handle))
                .expect("current runtime semantics fall back to the interpreter and complete");
        assert!(!result.times.is_empty(), "fallback run produced no samples");
        assert!(
            counters.total_failed() > 0,
            "the failing backend was never even attempted — the mutation fixture is vacuous"
        );
        assert_eq!(
            counters.total_succeeded(),
            0,
            "a backend that errors on every call cannot record native successes"
        );
        assert!(
            !native_success_evidence_holds(&counters),
            "silent interpreter fallback must not satisfy the native-success evidence"
        );
    }

    /// Backend differential discriminator over THIS bounded fixture: the same
    /// model, run under Auto (native) and Interpreter on the rk-like path.
    ///
    /// Scope stated plainly: this is a fixture-specific discriminator, NOT
    /// proof of a declared whole-run relation — no whole-trajectory
    /// native-vs-interpreter theorem is declared anywhere, and a row-local
    /// bound does not compose into one through an adaptive RK integrator.
    /// The value check applies the PRODUCTION per-row agreement formula,
    /// `validate_jit_matches_interpreter`
    /// (crates/rumoca-exec-cranelift/src/emit.rs:1148-1162): bit-equal or
    /// both-NaN passes outright, otherwise
    /// |actual − expected| <= 64 * EPSILON * max(|actual|, |expected|, 1) —
    /// scale-aware, not an absolute bound. On this short, well-conditioned
    /// fixture the two trajectories are expected to stay within that
    /// per-evaluation envelope at every published sample; a divergence beyond
    /// it flags a real backend semantics split for investigation.
    ///
    /// The structural claims that ARE general: bit-equal output time grids
    /// (both runs build the grid through the same backend-independent
    /// `timeline::try_build_output_times`), equal success status with no
    /// `terminate()`, and counted native success on the Auto leg so the
    /// comparison cannot pass vacuously as interpreter-vs-interpreter.
    #[cfg(feature = "solver-rk45")]
    #[test]
    fn rk45_auto_and_interpreter_agree_on_the_declared_relation() {
        let auto_opts = sim_opts(SimExecutionPolicy::Auto);
        let model = state_fixture(&auto_opts);
        let (_backend, counters, handle) = counting_handle(&model);
        let native =
            rumoca_solver_rk45::simulate_with_execution_backend(&model, &auto_opts, Some(handle))
                .expect("rk-like native run succeeds");
        assert!(
            counters.expression.succeeded.get() > 0,
            "the Auto leg never executed compiled expressions natively — the comparison \
             would be interpreter-vs-interpreter and vacuous"
        );
        assert_eq!(
            counters.total_failed(),
            0,
            "native calls failed on the Auto leg; the relation would be measured against \
             a partially interpreted run"
        );

        let interpreter_opts = sim_opts(SimExecutionPolicy::Interpreter);
        let interpreted =
            rumoca_solver_rk45::simulate_with_execution_backend(&model, &interpreter_opts, None)
                .expect("rk-like interpreter run succeeds");

        assert!(
            native.termination.is_none() && interpreted.termination.is_none(),
            "both runs must complete without a terminate() event"
        );
        assert_eq!(native.names, interpreted.names, "channel sets must match");
        assert_eq!(
            native.times.len(),
            interpreted.times.len(),
            "sample counts must match"
        );
        assert!(
            native
                .times
                .iter()
                .zip(&interpreted.times)
                .all(|(a, b)| a.to_bits() == b.to_bits()),
            "output time grids are built by the same backend-independent timeline code and \
             must be bit-equal"
        );
        // The production per-row agreement formula from
        // `validate_jit_matches_interpreter`
        // (crates/rumoca-exec-cranelift/src/emit.rs:1148-1162).
        fn agrees_per_production_formula(actual: f64, expected: f64) -> bool {
            if actual.to_bits() == expected.to_bits() || (actual.is_nan() && expected.is_nan()) {
                return true;
            }
            let scale = actual.abs().max(expected.abs()).max(1.0);
            (actual - expected).abs() <= f64::EPSILON * 64.0 * scale
        }
        for (channel, (native_row, interpreted_row)) in
            native.data.iter().zip(&interpreted.data).enumerate()
        {
            for (sample, (a, b)) in native_row.iter().zip(interpreted_row).enumerate() {
                assert!(
                    agrees_per_production_formula(*a, *b),
                    "channel {} ({}) sample {} diverges beyond the production scale-aware \
                     agreement formula (64*EPS*max(|a|,|b|,1)): native {} vs interpreter {}",
                    channel,
                    native.names.get(channel).map(String::as_str).unwrap_or("?"),
                    sample,
                    a,
                    b
                );
            }
        }
    }

    /// Discriminator (b): the interpreter path performs ZERO native calls.
    /// The composition gate withholds the handle for
    /// `SimExecutionPolicy::Interpreter`; a mutation that ignores the policy in
    /// the gate hands out a backend and fails the `None` assertion, and the
    /// interpreter run itself still completes without one.
    #[test]
    fn interpreter_policy_withholds_the_backend_and_still_simulates() {
        let opts = sim_opts(SimExecutionPolicy::Interpreter);
        let model = state_fixture(&opts);
        assert!(
            admitted_native_execution_backend(&opts, &model).is_none(),
            "the interpreter policy must withhold the native execution backend"
        );
        let auto_opts = sim_opts(SimExecutionPolicy::Auto);
        assert!(
            admitted_native_execution_backend(&auto_opts, &model).is_some(),
            "the auto policy must compose a native execution backend for a state-carrying model"
        );
        let result = rumoca_solver_diffsol::simulate_with_execution_backend(&model, &opts, None)
            .expect("interpreter BDF run succeeds without a backend");
        assert!(
            !result.times.is_empty(),
            "interpreter run produced no samples"
        );
    }

    /// Discriminator (c): the direct contradictory input — interpreter policy
    /// plus a supplied handle — is a typed rejection at the diffsol boundary,
    /// and the handle sees zero native activity of any kind. A mutation that
    /// silently executes (or silently withholds and continues) fails the
    /// variant match; one that touches the backend first fails the zero-count.
    #[test]
    fn interpreter_policy_with_handle_is_a_typed_rejection() {
        let interpreter_opts = sim_opts(SimExecutionPolicy::Interpreter);
        let model = state_fixture(&interpreter_opts);
        let (_backend, counters, handle) = counting_handle(&model);
        let error = rumoca_solver_diffsol::simulate_with_execution_backend(
            &model,
            &interpreter_opts,
            Some(handle),
        )
        .expect_err("interpreter policy plus a handle must be rejected, never executed");
        assert!(
            matches!(
                error.kind(),
                rumoca_solver_diffsol::SimError::ExecutionPolicyContradiction {
                    policy: "interpreter"
                }
            ),
            "expected the typed policy contradiction, got: {error}"
        );
        assert_eq!(
            counters.total_activity(),
            0,
            "the rejected handle must never be compiled against or called"
        );
    }

    /// Discriminator (c-rk45): the SAME contradictory input — interpreter
    /// policy plus a supplied handle — is typed-rejected on the rk-like path
    /// too, with zero backend activity. The admission rule is owned once by
    /// `rumoca_solver::fmi_me::admit_execution_backend` and enforced at the
    /// rk45 entry points themselves, so the public bypass around
    /// composition-time withholding in `rumoca-sim` is closed: a direct
    /// caller cannot obtain backend-dependent semantics for one request.
    #[cfg(feature = "solver-rk45")]
    #[test]
    fn rk45_interpreter_policy_with_handle_is_a_typed_rejection() {
        let interpreter_opts = sim_opts(SimExecutionPolicy::Interpreter);
        let model = state_fixture(&interpreter_opts);
        let (_backend, counters, handle) = counting_handle(&model);
        let error = rumoca_solver_rk45::simulate_with_execution_backend(
            &model,
            &interpreter_opts,
            Some(handle),
        )
        .expect_err("interpreter policy plus a handle must be rejected on the rk-like path too");
        assert!(
            matches!(
                &error,
                rumoca_solver_rk45::SimError::ExecutionPolicyContradiction(contradiction)
                    if contradiction.policy == "interpreter"
            ),
            "expected the shared typed policy contradiction, got: {error}"
        );
        assert_eq!(
            counters.total_activity(),
            0,
            "the rejected handle must never be compiled against or called on the rk-like path"
        );

        let (_session_backend, session_counters, session_handle) = counting_handle(&model);
        let session_error = rumoca_solver_rk45::SimulationSession::new_with_execution_backend(
            &model,
            interpreter_opts.clone(),
            Some(session_handle),
        )
        .err()
        .expect("the session entry point must reject the same contradiction");
        assert!(
            matches!(
                &session_error,
                rumoca_solver_rk45::SimError::ExecutionPolicyContradiction(contradiction)
                    if contradiction.policy == "interpreter"
            ),
            "expected the shared typed policy contradiction from the session entry, got: {session_error}"
        );
        assert_eq!(
            session_counters.total_activity(),
            0,
            "the session entry must not touch the rejected handle either"
        );
    }

    /// Discriminator (e-rk45, composition half): the rk-like path constructs
    /// NO backend for a zero-state model. All three rk45 composition call
    /// sites (`rk45::simulate_solve_model`, the stage-timing session build,
    /// and `rk45::SimulationSession::from_solve_model`) route through the ONE
    /// shared admission gate asserted here, which withholds — returning
    /// `None` before any backend is built — when `state_scalar_count() == 0`,
    /// so a pure-discrete request never pays Cranelift composition cost it
    /// cannot use. The state-carrying assertion keeps the check non-vacuous,
    /// and the full rk-like zero-state run proves the withheld path still
    /// completes.
    #[cfg(feature = "solver-rk45")]
    #[test]
    fn rk45_zero_state_composition_constructs_no_backend() {
        let opts = sim_opts(SimExecutionPolicy::Auto);
        let zero_model = zero_state_discrete_fixture(&opts);
        assert_eq!(
            zero_model.state_scalar_count(),
            0,
            "fixture must be zero-state"
        );
        assert!(
            admitted_native_execution_backend(&opts, &zero_model).is_none(),
            "the shared admission gate must withhold the backend for a zero-state model \
             under Auto — no backend may even be constructed"
        );
        let state_model = state_fixture(&opts);
        assert!(
            admitted_native_execution_backend(&opts, &state_model).is_some(),
            "the same gate must compose a backend for a state-carrying model under Auto, \
             or the zero-state assertion above is vacuous"
        );
        // The rk-like route that owns zero-state models is the session (the
        // batch entry maps `NoContinuousStates` to the typed `EmptySystem`
        // rejection); building it through the real rk45 composition call site
        // proves the withheld path still completes.
        crate::rk45::SimulationSession::from_solve_model(zero_model, opts)
            .expect("the rk-like zero-state session builds with the backend withheld");
    }

    /// Discriminator (e-rk45, ME-route half): even a handle deliberately
    /// forced past the composition gate reaches the rk-like ME route on a
    /// zero-state model with ZERO compile/call activity — the ME validator
    /// answers `NoContinuousStates` before any runtime compilation — and the
    /// handle is not retained once ME ownership ends, on either public rk45
    /// entry point.
    #[cfg(feature = "solver-rk45")]
    #[test]
    fn rk45_zero_state_force_supplied_handle_sees_no_activity_and_is_released() {
        let opts = sim_opts(SimExecutionPolicy::Auto);
        let model = zero_state_discrete_fixture(&opts);
        assert_eq!(model.state_scalar_count(), 0, "fixture must be zero-state");

        // Batch entry: the ME validator answers `NoContinuousStates` before
        // any runtime compilation, which this entry maps to the typed
        // `EmptySystem` rejection — with zero backend activity and no
        // retention even on the error path.
        let (backend, counters, handle) = counting_handle(&model);
        let error =
            rumoca_solver_rk45::simulate_with_execution_backend(&model, &opts, Some(handle))
                .expect_err("the rk-like batch entry rejects zero-state models as EmptySystem");
        assert!(
            matches!(&error, rumoca_solver_rk45::SimError::EmptySystem),
            "expected the typed EmptySystem rejection, got: {error}"
        );
        assert_eq!(
            counters.total_activity(),
            0,
            "the rk-like zero-state route must never compile against or call the backend"
        );
        assert_eq!(
            Rc::strong_count(&backend),
            1,
            "the rk-like zero-state route must not retain the unused backend"
        );

        let (session_backend, session_counters, session_handle) = counting_handle(&model);
        let session = rumoca_solver_rk45::SimulationSession::new_with_execution_backend(
            &model,
            opts.clone(),
            Some(session_handle),
        )
        .expect("rk-like zero-state session builds with a force-supplied handle");
        assert_eq!(
            session_counters.total_activity(),
            0,
            "the rk-like zero-state session must never compile against or call the backend"
        );
        drop(session);
        assert_eq!(
            Rc::strong_count(&session_backend),
            1,
            "after the zero-state session is dropped no other owner may retain the backend"
        );
    }

    /// FIX-5 pin (full prepared-sequence compile cardinality): building a
    /// prepared simulation performs the native compiles ONCE — eager owners
    /// at instantiation plus lazily compiled owners warmed by the build
    /// preflight — and the ENTIRE prepared lifecycle afterwards leaves EVERY
    /// compile counter unchanged: `check_initialization()`, a first `run()`,
    /// another interleaved `check_initialization()`, and a second `run()`.
    /// Native call successes (including the lazily compiled exact-assignment
    /// class) increase on BOTH runs. Both prepared operations consume the
    /// same `fresh_run_component` rewind of the same retained component, so
    /// an interleaved initialization check must ALSO leave the published run
    /// results bit-identical to a prepared simulation that never checked —
    /// proving the check mutates neither the template nor a later run's
    /// initial state.
    #[test]
    fn prepared_runs_reuse_compiled_native_programs_without_recompiling() {
        let opts = sim_opts(SimExecutionPolicy::Auto);
        let model = state_fixture(&opts);
        let (_backend, counters, handle) = counting_handle(&model);
        let prepared = rumoca_solver_diffsol::build_simulation_with_execution_backend(
            &model,
            &opts,
            Some(handle),
        )
        .expect("native BDF build succeeds");
        let compiles_after_build = counters.compile_counts();
        assert!(
            counters.expression_compiles.get() > 0,
            "the build performed no native expression compiles — the pin would be vacuous"
        );
        assert!(
            counters.assignment_compiles.get() > 0,
            "the build preflight compiled no lazy exact-assignment schedule — the lazy-path \
             half of this pin would be vacuous"
        );

        prepared
            .check_initialization()
            .expect("prepared initialization check succeeds");
        assert_eq!(
            counters.compile_counts(),
            compiles_after_build,
            "check_initialization() recompiled native programs the build already compiled"
        );
        let successes_after_check = counters.total_succeeded();
        let assignment_after_check = counters.assignment.succeeded.get();

        let first = prepared.run().expect("first prepared run succeeds");
        let successes_after_first = counters.total_succeeded();
        let assignment_after_first = counters.assignment.succeeded.get();
        assert!(
            successes_after_first > successes_after_check,
            "the first prepared run performed no native calls"
        );
        assert!(
            assignment_after_first > assignment_after_check,
            "the first run did not traverse the lazily compiled exact-assignment schedule"
        );
        assert_eq!(
            counters.compile_counts(),
            compiles_after_build,
            "the first hot run recompiled native programs the build already compiled"
        );

        prepared
            .check_initialization()
            .expect("interleaved initialization check succeeds");
        assert_eq!(
            counters.compile_counts(),
            compiles_after_build,
            "an interleaved check_initialization() recompiled native programs"
        );

        let second = prepared.run().expect("second prepared run succeeds");
        assert!(
            counters.total_succeeded() > successes_after_first,
            "the second prepared run performed no native calls"
        );
        assert!(
            counters.assignment.succeeded.get() > assignment_after_first,
            "the second run did not traverse the lazily compiled exact-assignment schedule; \
             the shared-or-precompiled lazy cache claim would be unproven"
        );
        assert_eq!(
            counters.compile_counts(),
            compiles_after_build,
            "the second hot run recompiled native programs the build already compiled"
        );
        assert_eq!(
            counters.total_failed(),
            0,
            "prepared native runs must not fail native calls into interpreter fallback"
        );

        // Interleaving immunity: a second prepared simulation over the same
        // model that never calls check_initialization() must publish
        // bit-identical trajectories for both runs.
        let (first_unchecked, second_unchecked) = unchecked_baseline_runs(&model, &opts);
        assert_run_unaffected_by_interleaved_check("first", &first, &first_unchecked);
        assert_run_unaffected_by_interleaved_check("second", &second, &second_unchecked);
    }

    /// Two runs of a freshly built prepared simulation that never calls
    /// `check_initialization()`, as the interleaving-immunity baseline.
    fn unchecked_baseline_runs(
        model: &solve::SolveModel,
        opts: &SimOptions,
    ) -> (rumoca_solver::SimResult, rumoca_solver::SimResult) {
        let (_backend, counters, handle) = counting_handle(model);
        let unchecked = rumoca_solver_diffsol::build_simulation_with_execution_backend(
            model,
            opts,
            Some(handle),
        )
        .expect("uncheck-path native BDF build succeeds");
        let first = unchecked.run().expect("first unchecked run succeeds");
        let second = unchecked.run().expect("second unchecked run succeeds");
        assert_eq!(
            counters.total_failed(),
            0,
            "unchecked native runs must not fail native calls"
        );
        (first, second)
    }

    fn assert_run_unaffected_by_interleaved_check(
        label: &str,
        checked: &rumoca_solver::SimResult,
        plain: &rumoca_solver::SimResult,
    ) {
        assert!(
            checked
                .times
                .iter()
                .zip(&plain.times)
                .all(|(a, b)| a.to_bits() == b.to_bits()),
            "{label} run time grids must be unaffected by an interleaved check_initialization()"
        );
        assert_eq!(
            checked.data.len(),
            plain.data.len(),
            "{label} run channel counts must match"
        );
        assert!(
            checked
                .data
                .iter()
                .zip(&plain.data)
                .all(|(left, right)| left
                    .iter()
                    .zip(right)
                    .all(|(a, b)| a.to_bits() == b.to_bits())),
            "{label} run published values must be unaffected by an interleaved \
             check_initialization() — the check mutated the template or a later run's \
             initial state"
        );
    }

    /// FIX-5 pin (one-shot compile cardinality): a one-shot simulate compiles
    /// exactly what one build compiles — each issued executable owner at most
    /// once, never once per phase. Before the prepared path retained its
    /// component, the one-shot chain (build-validate, then run) instantiated
    /// twice and compiled everything twice; this equality pin fails on any
    /// such regression.
    #[test]
    fn one_shot_simulate_compiles_each_owner_at_most_once() {
        let opts = sim_opts(SimExecutionPolicy::Auto);
        let model = state_fixture(&opts);
        let (_one_shot_backend, one_shot_counters, one_shot_handle) = counting_handle(&model);
        rumoca_solver_diffsol::simulate_with_execution_backend(
            &model,
            &opts,
            Some(one_shot_handle),
        )
        .expect("one-shot native BDF run succeeds");
        let (_build_backend, build_counters, build_handle) = counting_handle(&model);
        let _prepared = rumoca_solver_diffsol::build_simulation_with_execution_backend(
            &model,
            &opts,
            Some(build_handle),
        )
        .expect("native BDF build succeeds");
        assert_eq!(
            one_shot_counters.compile_counts(),
            build_counters.compile_counts(),
            "a one-shot simulate must compile exactly what one build compiles; more means \
             some issued owner was compiled again for the run phase"
        );
    }

    /// FIX-5 cross-model pin: two different models prepared and run through
    /// the SAME backend factory instance must never reuse each other's
    /// compiled programs. There is deliberately NO factory-level compile
    /// cache (no implemented root identity exists to key one safely), so the
    /// second model must perform its own nonzero compiles, and both runs must
    /// stay correct.
    #[test]
    fn same_backend_factory_serves_two_models_without_cross_reuse() {
        let opts = sim_opts(SimExecutionPolicy::Auto);
        let model_a = state_fixture(&opts);
        let model_b = lower(
            concat!(
                "model NativeSecondModel\n",
                "  Real a(start = 2, fixed = true);\n",
                "  Real w(start = 1);\n",
                "equation\n",
                "  der(a) = -0.5 * a + w;\n",
                "  w + 0.2 * sin(w) = a;\n",
                "end NativeSecondModel;\n",
            ),
            "NativeSecondModel",
            &opts,
        );
        // ONE counting factory instance serving both models (neither fixture
        // uses typed pure calls, so the shared inner backend is valid for
        // both), wrapped into one opaque handle per model.
        let counters = Rc::new(NativeCallCounters::default());
        let factory = Rc::new(CountingBackend {
            inner: crate::native_execution::backend(&model_a.pure_calls),
            counters: counters.clone(),
        });
        let handle_a = MeExecutionBackend::new(factory.clone() as Rc<dyn SolveExecutionBackend>);
        let handle_b = MeExecutionBackend::new(factory.clone() as Rc<dyn SolveExecutionBackend>);

        let prepared_a = rumoca_solver_diffsol::build_simulation_with_execution_backend(
            &model_a,
            &opts,
            Some(handle_a),
        )
        .expect("first model builds");
        let result_a = prepared_a.run().expect("first model runs");
        let compiles_after_a = counters.compile_counts();
        assert!(
            counters.expression_compiles.get() > 0,
            "the first model performed no native compiles — the pin would be vacuous"
        );

        let prepared_b = rumoca_solver_diffsol::build_simulation_with_execution_backend(
            &model_b,
            &opts,
            Some(handle_b),
        )
        .expect("second model builds");
        let result_b = prepared_b.run().expect("second model runs");
        assert!(
            counters.compile_counts() != compiles_after_a,
            "the second model reused the first model's compiled programs: a factory-level \
             cache with no checked root identity is forbidden"
        );
        for (result, label) in [(&result_a, "first"), (&result_b, "second")] {
            assert!(
                !result.times.is_empty(),
                "{label} model produced no samples"
            );
            assert!(
                result
                    .data
                    .iter()
                    .all(|row| row.iter().all(|value| value.is_finite())),
                "{label} model produced non-finite values"
            );
        }
        assert_eq!(
            counters.total_failed(),
            0,
            "cross-model native runs must not fail native calls into interpreter fallback"
        );
    }

    /// Discriminator (d): the opaque handle is not retained beyond ME
    /// ownership. While a prepared simulation lives it may hold the backend;
    /// after the run and drop, the test's own Rc is the only owner left. A
    /// mutation that stashes a second long-lived owner (global, leak, cache
    /// outside the component) fails the final strong-count assertion.
    #[test]
    fn opaque_handle_is_released_after_me_ownership_ends() {
        let opts = sim_opts(SimExecutionPolicy::Auto);
        let model = state_fixture(&opts);
        let (backend, _counters, handle) = counting_handle(&model);
        let prepared = rumoca_solver_diffsol::build_simulation_with_execution_backend(
            &model,
            &opts,
            Some(handle),
        )
        .expect("native BDF build succeeds");
        assert!(
            Rc::strong_count(&backend) >= 2,
            "the prepared simulation should hold the backend while it lives"
        );
        prepared.run().expect("prepared native BDF run succeeds");
        drop(prepared);
        assert_eq!(
            Rc::strong_count(&backend),
            1,
            "after ME ownership ends no other owner may retain the execution backend"
        );
    }

    /// Discriminator (e): the zero-state path is explicitly tested. The
    /// composition gate never constructs (pays for) a backend for a
    /// zero-state model, and even a handle forced past the gate is never
    /// compiled against or called by the no-state session.
    #[test]
    fn zero_state_path_constructs_and_uses_no_backend() {
        let opts = sim_opts(SimExecutionPolicy::Auto);
        let model = zero_state_fixture(&opts);
        assert_eq!(model.state_scalar_count(), 0, "fixture must be zero-state");
        assert!(
            admitted_native_execution_backend(&opts, &model).is_none(),
            "a zero-state model must not pay for a native backend it cannot use"
        );
        let (backend, counters, handle) = counting_handle(&model);
        let result =
            rumoca_solver_diffsol::simulate_with_execution_backend(&model, &opts, Some(handle))
                .expect("zero-state run succeeds");
        assert!(
            !result.times.is_empty(),
            "zero-state run produced no samples"
        );
        assert_eq!(
            counters.total_activity(),
            0,
            "the zero-state path must never compile against or call the backend"
        );
        assert_eq!(
            Rc::strong_count(&backend),
            1,
            "the zero-state path must not retain the unused backend"
        );
    }
}
