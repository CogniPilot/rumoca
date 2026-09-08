#[cfg(feature = "scheduled-sim")]
use indexmap::IndexMap;

use crate::SimError;
#[cfg(feature = "scheduled-sim")]
use crate::SimulationSessionApi;
use crate::me_backend::BackendSimulationSession;
use crate::simulation_session::SessionState;
use crate::solve_lowering::SimulationDiagnosticError;

// Native-backend composition for the BDF host goes through the ONE shared
// sim-side admission gate,
// [`crate::native_execution::admitted_native_execution_backend`], exactly as
// on the rk-like path: `rumoca-solver-diffsol` only ever receives the opaque
// handle and can neither construct nor unwrap a backend of its own, and the
// interpreter-policy / zero-state withholding rules cannot drift between the
// two concrete paths.

pub(crate) struct SimulationSession {
    inner: BackendSimulationSession,
}

impl SimulationSession {
    /// Build from one checked correlated FMI artifact, preserving the same
    /// component across solver selection and session construction.
    pub(crate) fn from_artifact(
        artifact: rumoca_solver::fmi_me::MeModelArtifact,
        opts: rumoca_solver::SimOptions,
        execution_backend: Option<rumoca_solver::fmi_me::MeExecutionBackend>,
    ) -> Result<Self, SimulationDiagnosticError> {
        let inner = BackendSimulationSession::new(
            artifact,
            &opts,
            execution_backend,
            "diffsol",
            rumoca_solver_diffsol::model_exchange_integrator,
        )
        .map_err(SimulationDiagnosticError::from)?;
        Ok(Self { inner })
    }

    #[cfg(all(feature = "solver-diffsol", feature = "solver-rk45"))]
    pub(crate) fn from_retained(
        retained: rumoca_solver::fmi_me::session::MeRetainedComponent,
        opts: rumoca_solver::SimOptions,
    ) -> Result<Self, SimulationDiagnosticError> {
        let inner = BackendSimulationSession::from_retained(
            retained,
            &opts,
            rumoca_solver_diffsol::model_exchange_integrator,
        )
        .map_err(SimulationDiagnosticError::from)?;
        Ok(Self { inner })
    }

    pub(crate) fn set_input(&mut self, name: &str, value: f64) -> Result<(), SimError> {
        self.inner.set_input(name, value)
    }

    pub(crate) fn advance_to(&mut self, target_time: f64) -> Result<(), SimError> {
        self.inner.advance_to(target_time)
    }

    pub(crate) fn step(&mut self, dt: f64) -> Result<(), SimError> {
        if dt <= 0.0 {
            return Ok(());
        }
        self.advance_to(self.time() + dt)
    }

    pub(crate) fn reset(&mut self) -> Result<(), SimError> {
        self.inner.reset()
    }

    pub(crate) fn retime(&mut self, t_start: f64) -> Result<(), SimError> {
        self.inner.retime(t_start)
    }

    pub(crate) fn time(&self) -> f64 {
        self.inner.time()
    }

    pub(crate) fn get(&self, name: &str) -> Result<Option<f64>, SimError> {
        self.inner.get(name)
    }

    pub(crate) fn state(&self) -> Result<SessionState, SimError> {
        Ok(SessionState {
            time: self.time(),
            values: self.inner.visible_values()?,
        })
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
        None
    }
}

#[cfg(feature = "scheduled-sim")]
impl SimulationSessionApi for SimulationSession {
    type Error = SimError;

    fn retime(&mut self, t_start: f64) -> Result<(), Self::Error> {
        Self::retime(self, t_start)
    }

    fn set_input(&mut self, name: &str, value: f64) -> Result<(), Self::Error> {
        Self::set_input(self, name, value)
    }

    fn advance_to(&mut self, target_time: f64) -> Result<(), Self::Error> {
        Self::advance_to(self, target_time)
    }

    fn time(&self) -> f64 {
        Self::time(self)
    }

    fn values_for(&self, names: &[String]) -> Result<IndexMap<String, f64>, Self::Error> {
        Self::values_for(self, names)
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
    use std::sync::Arc;

    use rumoca_compile::compile::{Session, SessionConfig};
    use rumoca_ir_solve as solve;
    use rumoca_solver::{
        CompiledSolveAssignmentSchedule, CompiledSolveEventTransaction, CompiledSolveExpression,
        CompiledSolveJacobianExpression, SimExecutionPolicy, SimOptions, SolveExecutionBackend,
        fmi_me::MeExecutionBackend,
    };

    use crate::SimError;
    use crate::native_execution::admitted_native_execution_backend;

    fn prepare_test_artifact(
        artifact: rumoca_solver::fmi_me::MeModelArtifact,
        opts: &SimOptions,
        execution_backend: Option<MeExecutionBackend>,
    ) -> Result<crate::PreparedSimulation, SimError> {
        crate::prepared_simulation::prepare_artifact(artifact, opts, execution_backend)
    }

    fn run_test_artifact(
        artifact: rumoca_solver::fmi_me::MeModelArtifact,
        opts: &SimOptions,
        execution_backend: Option<MeExecutionBackend>,
    ) -> Result<rumoca_solver::SimResult, SimError> {
        prepare_test_artifact(artifact, opts, execution_backend)
            .and_then(crate::PreparedSimulation::run)
    }

    /// Attempted / succeeded / failed accounting for one native call class.
    ///
    /// Success is recorded only after the delegated call returns `Ok`; a native
    /// call failure is terminal and is recorded separately.
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
        /// attempts. Typed policy rejections assert this is zero because a
        /// rejected handle must never be touched.
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
    /// this, however many attempts it logs.
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
        fn call(&self, y: &[f64], p: &[f64], t: f64, out: &mut [f64]) -> Result<(), String> {
            let result = self.inner.call(y, p, t, out);
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
            out: &mut [f64],
        ) -> Result<(), String> {
            let result = self.inner.call(y, p, t, seed, out);
            self.counters.jacobian.observe(&result);
            result
        }
    }

    struct CountingAssignment {
        inner: Rc<dyn CompiledSolveAssignmentSchedule>,
        counters: Rc<NativeCallCounters>,
    }

    impl CompiledSolveAssignmentSchedule for CountingAssignment {
        fn call(&self, y: &mut [f64], p: &[f64], t: f64) -> Result<(), String> {
            let result = self.inner.call(y, p, t);
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
            execution: &solve::ExactRefreshAssignmentExecution<'_>,
        ) -> Result<Rc<dyn CompiledSolveAssignmentSchedule>, String> {
            NativeCallCounters::bump(&self.counters.assignment_compiles);
            self.inner
                .compile_assignment_schedule(execution)
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

    struct ModelFixture {
        model: Arc<solve::SolveModel>,
        component_wire: String,
    }

    impl std::ops::Deref for ModelFixture {
        type Target = solve::SolveModel;

        fn deref(&self) -> &Self::Target {
            self.model.as_ref()
        }
    }

    impl ModelFixture {
        fn component(&self) -> solve::fmi::FmiComponent {
            let mut deserializer = serde_json::Deserializer::from_str(&self.component_wire);
            rumoca_phase_solve::fmi::deserialize_fmi_component(&mut deserializer)
                .expect("fixture FMI component replays")
        }

        fn artifact(&self) -> rumoca_solver::fmi_me::MeModelArtifact {
            rumoca_solver::fmi_me::MeModelArtifact::new(self.component())
        }

        fn shared_model(&self) -> Arc<solve::SolveModel> {
            Arc::clone(&self.model)
        }
    }

    fn lower(source: &str, model: &str, opts: &SimOptions) -> ModelFixture {
        let mut session = Session::new(SessionConfig::default());
        session
            .add_document("native_policy_fixture.mo", source)
            .expect("fixture parses");
        let dae = session
            .compile_model(model)
            .expect("fixture compiles through checked ToDAE")
            .dae;
        let lowered =
            crate::solve_lowering::lower_correlated_for_simulation_with_overrides(&dae, opts)
                .expect("fixture lowers to a correlated Solve model");
        let wire = rumoca_phase_solve::fmi::fmi_component_wire(&lowered)
            .expect("fixture FMI wire constructs");
        let component_wire = serde_json::to_string(&wire).expect("fixture FMI wire serializes");
        let model = Arc::new(lowered.into_model());
        ModelFixture {
            model,
            component_wire,
        }
    }

    /// One state solved by a *nonlinear* initial equation (initialization
    /// Newton, whose Jacobian-vector products go through the compiled JVP),
    /// one nonlinear algebraic (`y`), and one affine algebraic (`z`, exact
    /// assignment schedule), so a single native run must exercise compiled
    /// expression, compiled JVP, and compiled exact-assignment calls.
    fn state_fixture(opts: &SimOptions) -> ModelFixture {
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

    #[cfg(all(feature = "solver-rk45", feature = "scheduled-sim"))]
    fn initialization_termination_fixture(opts: &SimOptions) -> ModelFixture {
        lower(
            concat!(
                "model InitialTermination\n",
                "  Real x(start = 3.0, fixed = true);\n",
                "equation\n",
                "  der(x) = -x;\n",
                "  when initial() then\n",
                "    terminate(\"terminated during initialization\");\n",
                "  end when;\n",
                "end InitialTermination;\n",
            ),
            "InitialTermination",
            opts,
        )
    }

    /// The value path is the finite equilibrium `x = z = 0`, but the local
    /// algebraic sensitivity of `z*z = x` requires solving `0*dz = dx` at that
    /// point. The production FMI directional-derivative call must therefore
    /// refuse BDF eligibility while an explicit host can execute the values.
    #[cfg(feature = "solver-rk45")]
    fn finite_value_singular_linearization_fixture(opts: &SimOptions) -> ModelFixture {
        lower(
            concat!(
                "model FiniteValueSingularLinearization\n",
                "  Real x(start = 0, fixed = true);\n",
                "  Real z(start = 0);\n",
                "equation\n",
                "  der(x) = z;\n",
                "  z * z = x;\n",
                "end FiniteValueSingularLinearization;\n",
            ),
            "FiniteValueSingularLinearization",
            opts,
        )
    }

    fn zero_state_fixture(opts: &SimOptions) -> ModelFixture {
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
    #[cfg(feature = "solver-rk45")]
    fn zero_state_discrete_fixture(opts: &SimOptions) -> ModelFixture {
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
        counting_handle_over(
            crate::native_execution::backend(model.pure_calls())
                .expect("fixture pure-call table compiles"),
        )
    }

    /// Read-only failure fixture: expression and JVP compilation succeeds, but
    /// their selected calls fail terminally.
    struct FailingCompiled;

    impl CompiledSolveExpression for FailingCompiled {
        fn call(&self, _y: &[f64], _p: &[f64], _t: f64, _out: &mut [f64]) -> Result<(), String> {
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
            _out: &mut [f64],
        ) -> Result<(), String> {
            Err("injected native JVP failure".to_string())
        }
    }

    struct FailingBackend {
        inner: Rc<dyn SolveExecutionBackend>,
    }

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
            execution: &solve::ExactRefreshAssignmentExecution<'_>,
        ) -> Result<Rc<dyn CompiledSolveAssignmentSchedule>, String> {
            self.inner.compile_assignment_schedule(execution)
        }

        fn compile_event_transaction(
            &self,
            program: &solve::EventTransactionProgram,
        ) -> Result<Rc<dyn CompiledSolveEventTransaction>, String> {
            self.inner.compile_event_transaction(program)
        }
    }

    struct CompileFailingBackend;

    impl SolveExecutionBackend for CompileFailingBackend {
        fn compile_expression(
            &self,
            _block: &solve::ScalarProgramBlock,
        ) -> Result<Rc<dyn CompiledSolveExpression>, String> {
            Err("injected native compile failure".to_string())
        }

        fn compile_jacobian_expression(
            &self,
            _block: &solve::ScalarProgramBlock,
        ) -> Result<Rc<dyn CompiledSolveJacobianExpression>, String> {
            Err("injected native compile failure".to_string())
        }

        fn compile_assignment_schedule(
            &self,
            _execution: &solve::ExactRefreshAssignmentExecution<'_>,
        ) -> Result<Rc<dyn CompiledSolveAssignmentSchedule>, String> {
            Err("injected native compile failure".to_string())
        }

        fn compile_event_transaction(
            &self,
            _program: &solve::EventTransactionProgram,
        ) -> Result<Rc<dyn CompiledSolveEventTransaction>, String> {
            Err("injected native compile failure".to_string())
        }
    }

    #[test]
    fn native_compile_failure_rejects_before_a_runtime_exists() {
        let opts = sim_opts(SimExecutionPolicy::Auto);
        let model = state_fixture(&opts);
        let error = match rumoca_solver::SolveRuntime::new_native(
            model.shared_model(),
            &CompileFailingBackend,
        ) {
            Ok(_) => panic!("a failed required native compile must reject preparation"),
            Err(error) => error,
        };
        assert!(matches!(
            error,
            rumoca_solver::RuntimeSolveError::NativeExecution {
                stage: rumoca_solver::NativeExecutionStage::Compile,
                owner: rumoca_solver::NativeExecutionOwner::ImplicitResidual,
                reason,
            } if reason == "injected native compile failure"
        ));
    }

    struct MutatingAssignmentBackend {
        inner: Rc<dyn SolveExecutionBackend>,
        assignment_calls: Rc<Cell<usize>>,
    }

    struct MutatingFailAssignment {
        calls: Rc<Cell<usize>>,
    }

    impl CompiledSolveAssignmentSchedule for MutatingFailAssignment {
        fn call(&self, y: &mut [f64], _p: &[f64], _t: f64) -> Result<(), String> {
            self.calls.set(self.calls.get() + 1);
            y.fill(f64::NAN);
            Err("injected mutating assignment failure".to_string())
        }
    }

    impl SolveExecutionBackend for MutatingAssignmentBackend {
        fn compile_expression(
            &self,
            block: &solve::ScalarProgramBlock,
        ) -> Result<Rc<dyn CompiledSolveExpression>, String> {
            self.inner.compile_expression(block)
        }

        fn compile_jacobian_expression(
            &self,
            block: &solve::ScalarProgramBlock,
        ) -> Result<Rc<dyn CompiledSolveJacobianExpression>, String> {
            self.inner.compile_jacobian_expression(block)
        }

        fn compile_assignment_schedule(
            &self,
            _execution: &solve::ExactRefreshAssignmentExecution<'_>,
        ) -> Result<Rc<dyn CompiledSolveAssignmentSchedule>, String> {
            Ok(Rc::new(MutatingFailAssignment {
                calls: self.assignment_calls.clone(),
            }))
        }

        fn compile_event_transaction(
            &self,
            program: &solve::EventTransactionProgram,
        ) -> Result<Rc<dyn CompiledSolveEventTransaction>, String> {
            self.inner.compile_event_transaction(program)
        }
    }

    #[test]
    fn mutating_native_assignment_failure_is_terminal_and_atomic() {
        let opts = sim_opts(SimExecutionPolicy::Auto);
        let model = state_fixture(&opts);
        let assignment_calls = Rc::new(Cell::new(0));
        let backend = MutatingAssignmentBackend {
            inner: crate::native_execution::backend(model.pure_calls())
                .expect("fixture pure-call table compiles"),
            assignment_calls: assignment_calls.clone(),
        };
        let runtime = rumoca_solver::SolveRuntime::new_native(model.shared_model(), &backend)
            .expect("all native obligations compile");
        let mut y = model.initial_y().to_vec();
        let incoming = y.clone();
        let error = runtime
            .refresh_algebraic_and_output_slots(
                opts.t_start,
                &mut y,
                model.parameters(),
                opts.atol,
                20,
            )
            .expect_err("the selected mutating assignment arm must fail terminally");
        assert!(matches!(
            error,
            rumoca_solver::RuntimeSolveError::NativeExecution {
                stage: rumoca_solver::NativeExecutionStage::Call,
                owner: rumoca_solver::NativeExecutionOwner::ExactAssignment { .. },
                reason,
            } if reason == "injected mutating assignment failure"
        ));
        assert!(assignment_calls.get() > 0, "assignment witness is vacuous");
        assert_eq!(
            y.iter().map(|value| value.to_bits()).collect::<Vec<_>>(),
            incoming
                .iter()
                .map(|value| value.to_bits())
                .collect::<Vec<_>>(),
            "a failed native assignment exposed partially mutated solver state"
        );
    }

    #[test]
    fn explicitly_selected_interpreter_remains_a_positive_control() {
        let opts = sim_opts(SimExecutionPolicy::Interpreter);
        let model = state_fixture(&opts);
        let result = run_test_artifact(model.artifact(), &opts, None)
            .expect("the explicitly selected interpreter executes successfully");
        assert!(!result.times.is_empty());
    }

    /// Discriminator (a): the Auto/native BDF path really executes compiled
    /// expression, JVP, and exact-assignment native calls — SUCCESSFULLY. The
    /// each class asserts `succeeded > 0` (recorded only after the delegated
    /// call returns `Ok`) AND `failed == 0`.
    #[test]
    fn auto_native_bdf_path_performs_counted_native_calls() {
        let opts = sim_opts(SimExecutionPolicy::Auto);
        let model = state_fixture(&opts);
        assert!(
            matches!(
                model
                    .problem()
                    .continuous()
                    .refresh_owners()
                    .algebraic()
                    .value_stages(),
                [
                    rumoca_ir_solve::IssuedRefreshStage::ProjectionBlock { seed_rows, .. },
                    rumoca_ir_solve::IssuedRefreshStage::ExactAssignments { dynamic_rows, .. },
                ] if seed_rows.is_empty() && !dynamic_rows.is_empty()
            ),
            "the native witness requires an executable projection followed by an exact assignment, \
         with no optional projection warm-start row"
        );
        let (_backend, counters, handle) = counting_handle(&model);
        let result = run_test_artifact(model.artifact(), &opts, Some(handle))
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
                "compiled {class} calls failed unexpectedly: attempted={} \
                 succeeded={}",
                counter.attempted.get(),
                counter.succeeded.get()
            );
        }
        assert_eq!(
            counters.event_transaction.failed.get(),
            0,
            "compiled event-transaction calls failed unexpectedly"
        );
        assert!(
            native_success_evidence_holds(&counters),
            "the native-success evidence predicate must agree with the per-class assertions"
        );
    }

    #[cfg(feature = "solver-rk45")]
    #[test]
    fn auto_bdf_probe_and_run_share_one_eagerly_compiled_component() {
        let opts = sim_opts(SimExecutionPolicy::Auto);
        let model = state_fixture(&opts);
        let (_backend, counters, handle) = counting_handle(&model);
        let prepared = prepare_test_artifact(model.artifact(), &opts, Some(handle))
            .expect("the BDF-capable fixture is prepared once");
        assert_eq!(prepared.backend(), rumoca_solver::SimBackend::Diffsol);
        let compiles_after_probe = counters.compile_counts();
        assert!(
            compiles_after_probe.into_iter().sum::<usize>() > 0,
            "the eager-compile witness is vacuous"
        );

        let result = prepared
            .run()
            .expect("the probed retained component executes without re-instantiation");

        assert!(!result.times.is_empty());
        assert_eq!(
            counters.compile_counts(),
            compiles_after_probe,
            "auto execution instantiated and eagerly compiled the FMI component twice"
        );
    }

    #[cfg(feature = "solver-rk45")]
    #[test]
    fn auto_directional_refusal_rewinds_the_probed_instance_for_the_explicit_host() {
        let opts = sim_opts(SimExecutionPolicy::Interpreter);
        let model = finite_value_singular_linearization_fixture(&opts);
        let prepared = prepare_test_artifact(model.artifact(), &opts, None)
            .expect("the real directional refusal is an integrator capability result");
        assert_eq!(prepared.backend(), rumoca_solver::SimBackend::Rk45);

        // The capability probe initialized the sole instance and evaluated a
        // real FMI directional derivative. Reusing it without the consuming
        // lease's pristine rewind would begin from the wrong lifecycle state.
        let result = prepared
            .run()
            .expect("the explicit host executes the finite value path from pristine state");
        assert!(
            !result.times.is_empty(),
            "the explicit witness produced no trace"
        );
        assert_eq!(
            result.times.last().map(|time| time.to_bits()),
            Some(opts.t_end.to_bits()),
            "the explicit host must complete the selected trajectory, not only reinitialize"
        );
        assert!(
            result.times.iter().all(|value| value.is_finite())
                && result.data.iter().flatten().all(|value| value.is_finite()),
            "the explicitly selected value path must remain finite"
        );
    }

    #[cfg(feature = "solver-rk45")]
    #[test]
    fn explicit_solver_modes_do_not_enter_the_auto_capability_probe() {
        use crate::prepared_simulation::{
            verification_bdf_capability_probe_count, verification_reset_bdf_capability_probes,
        };

        verification_reset_bdf_capability_probes();

        let mut bdf_opts = sim_opts(SimExecutionPolicy::Interpreter);
        bdf_opts.solver_mode = rumoca_solver::SimSolverMode::Bdf;
        let bdf_model = state_fixture(&bdf_opts);
        run_test_artifact(bdf_model.artifact(), &bdf_opts, None)
            .expect("explicit BDF bypasses capability probing and runs");
        assert_eq!(
            verification_bdf_capability_probe_count(),
            0,
            "explicit BDF must not enter Auto's capability-probe lifecycle"
        );

        let mut rk_opts = sim_opts(SimExecutionPolicy::Interpreter);
        rk_opts.solver_mode = rumoca_solver::SimSolverMode::RkLike;
        let rk_model = state_fixture(&rk_opts);
        run_test_artifact(rk_model.artifact(), &rk_opts, None)
            .expect("explicit rk-like bypasses capability probing and runs");
        assert_eq!(
            verification_bdf_capability_probe_count(),
            0,
            "explicit rk-like must not enter Auto's capability-probe lifecycle"
        );

        let auto_opts = sim_opts(SimExecutionPolicy::Interpreter);
        let auto_model = state_fixture(&auto_opts);
        run_test_artifact(auto_model.artifact(), &auto_opts, None)
            .expect("Auto performs its capability probe and runs the selected component");
        assert_eq!(
            verification_bdf_capability_probe_count(),
            1,
            "Auto must enter the capability-probe lifecycle exactly once"
        );
    }

    #[cfg(feature = "solver-rk45")]
    #[test]
    fn parameterless_reset_replays_nonzero_component_start_on_both_selected_backends() {
        let mut opts = sim_opts(SimExecutionPolicy::Interpreter);
        opts.t_start = 0.25;
        opts.t_end = 0.75;
        let model = state_fixture(&opts);

        let mut bdf = super::SimulationSession::from_artifact(model.artifact(), opts.clone(), None)
            .expect("the BDF session starts from the checked component");
        bdf.advance_to(0.5).expect("BDF advances away from start");
        bdf.reset().expect("BDF replays pristine");
        assert_eq!(bdf.time().to_bits(), opts.t_start.to_bits());

        let mut rk_like = crate::rk45::SimulationSession::from_selected_artifact(
            model.artifact(),
            opts.clone(),
            None,
        )
        .expect("the rk-like session starts from the checked component");
        rk_like
            .advance_to(0.5)
            .expect("rk-like advances away from start");
        rk_like.reset().expect("rk-like replays pristine");
        assert_eq!(rk_like.time().to_bits(), opts.t_start.to_bits());
    }

    #[cfg(all(feature = "solver-rk45", feature = "scheduled-sim"))]
    #[test]
    fn scheduled_facade_preserves_each_direct_backends_explicit_policy() {
        use crate::SimulationSessionApi;

        let opts = sim_opts(SimExecutionPolicy::Interpreter);
        let model = state_fixture(&opts);
        let names = Vec::<String>::new();

        let direct_bdf =
            super::SimulationSession::from_artifact(model.artifact(), opts.clone(), None)
                .expect("the direct BDF session initializes");
        let direct_bdf_values = SimulationSessionApi::values_for(&direct_bdf, &names)
            .expect("the direct BDF batch-value policy succeeds");
        let direct_bdf_cap = SimulationSessionApi::max_schedule_advance_dt(&direct_bdf);
        assert_eq!(direct_bdf_cap, None, "Diffsol declares no schedule cap");
        let facade_bdf = crate::SimulationSession::verification_from_diffsol(direct_bdf);
        assert_eq!(
            SimulationSessionApi::values_for(&facade_bdf, &names)
                .expect("the facade BDF batch-value policy succeeds"),
            direct_bdf_values
        );
        assert_eq!(
            SimulationSessionApi::max_schedule_advance_dt(&facade_bdf),
            direct_bdf_cap
        );

        let direct_rk =
            crate::rk45::SimulationSession::from_selected_artifact(model.artifact(), opts, None)
                .expect("the direct rk-like session initializes");
        let direct_rk_values = SimulationSessionApi::values_for(&direct_rk, &names)
            .expect("the direct rk-like batch-value policy succeeds");
        let direct_rk_cap = SimulationSessionApi::max_schedule_advance_dt(&direct_rk);
        assert_eq!(direct_rk_cap, None, "RK declares no schedule cap");
        let facade_rk = crate::SimulationSession::verification_from_rk_like(direct_rk);
        assert_eq!(
            SimulationSessionApi::values_for(&facade_rk, &names)
                .expect("the facade rk-like batch-value policy succeeds"),
            direct_rk_values
        );
        assert_eq!(
            SimulationSessionApi::max_schedule_advance_dt(&facade_rk),
            direct_rk_cap
        );
    }

    #[cfg(all(feature = "solver-rk45", feature = "scheduled-sim"))]
    #[test]
    fn terminal_final_reads_survive_direct_backends_and_scheduled_facades() {
        use crate::SimulationSessionApi;

        let opts = sim_opts(SimExecutionPolicy::Interpreter);
        let model = initialization_termination_fixture(&opts);
        let names = vec!["x".to_owned()];

        let mut direct_bdf =
            super::SimulationSession::from_artifact(model.artifact(), opts.clone(), None)
                .expect("the direct BDF session retains an initialization terminal point");
        assert_eq!(direct_bdf.get("x").unwrap(), Some(3.0));
        assert_eq!(direct_bdf.state().unwrap().values["x"], 3.0);
        assert_eq!(direct_bdf.values_for(&names).unwrap()["x"], 3.0);
        let time = direct_bdf.time();
        direct_bdf
            .advance_to(0.5)
            .expect("a terminal advance is an idempotent observation boundary");
        assert_eq!(direct_bdf.time().to_bits(), time.to_bits());
        assert!(direct_bdf.set_input("x", 4.0).is_err());
        assert_eq!(direct_bdf.get("x").unwrap(), Some(3.0));

        let direct_rk =
            crate::rk45::SimulationSession::from_selected_artifact(model.artifact(), opts, None)
                .expect("the direct rk-like session retains an initialization terminal point");
        assert_eq!(direct_rk.get("x").unwrap(), Some(3.0));
        assert_eq!(direct_rk.state().unwrap().values["x"], 3.0);
        assert_eq!(direct_rk.values_for(&names).unwrap()["x"], 3.0);

        let facade_bdf = crate::SimulationSession::verification_from_diffsol(direct_bdf);
        assert_eq!(facade_bdf.get("x").unwrap(), Some(3.0));
        assert_eq!(
            SimulationSessionApi::values_for(&facade_bdf, &names).unwrap()["x"],
            3.0
        );
        let facade_rk = crate::SimulationSession::verification_from_rk_like(direct_rk);
        assert_eq!(facade_rk.get("x").unwrap(), Some(3.0));
        assert_eq!(
            SimulationSessionApi::values_for(&facade_rk, &names).unwrap()["x"],
            3.0
        );
    }

    /// A read-only native call failure is terminal; it cannot become backend
    /// absence or authorize an interpreter retry.
    #[test]
    fn native_call_failure_is_typed_terminal() {
        let opts = sim_opts(SimExecutionPolicy::Auto);
        let model = state_fixture(&opts);
        let (_backend, counters, handle) = counting_handle_over(Rc::new(FailingBackend {
            inner: crate::native_execution::backend(model.pure_calls())
                .expect("fixture pure-call table compiles"),
        }));
        let error = run_test_artifact(model.artifact(), &opts, Some(handle))
            .expect_err("a selected native call failure must terminate the run");
        assert!(
            matches!(
                error.kind(),
                SimError::NativeExecution {
                    execution_stage: rumoca_solver::NativeExecutionStage::Call,
                    owner: rumoca_solver::NativeExecutionOwner::InitialResidual,
                    reason,
                } if reason == "injected native expression failure"
            ),
            "expected the exact typed native-call failure, got {error}"
        );
        assert!(
            counters.total_failed() > 0,
            "the failing backend was never even attempted — the mutation fixture is vacuous"
        );
        assert_eq!(counters.expression.succeeded.get(), 0);
        assert!(!native_success_evidence_holds(&counters));
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
        let mut auto_opts = sim_opts(SimExecutionPolicy::Auto);
        auto_opts.solver_mode = rumoca_solver::SimSolverMode::RkLike;
        let model = state_fixture(&auto_opts);
        let (_backend, counters, handle) = counting_handle(&model);
        let native = run_test_artifact(model.artifact(), &auto_opts, Some(handle))
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

        let mut interpreter_opts = sim_opts(SimExecutionPolicy::Interpreter);
        interpreter_opts.solver_mode = rumoca_solver::SimSolverMode::RkLike;
        let interpreted = run_test_artifact(model.artifact(), &interpreter_opts, None)
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
            admitted_native_execution_backend(&opts, &model)
                .expect("interpreter admission succeeds")
                .is_none(),
            "the interpreter policy must withhold the native execution backend"
        );
        let auto_opts = sim_opts(SimExecutionPolicy::Auto);
        assert!(
            admitted_native_execution_backend(&auto_opts, &model)
                .expect("native admission succeeds")
                .is_some(),
            "the auto policy must compose a native execution backend for a state-carrying model"
        );
        let result = run_test_artifact(model.artifact(), &opts, None)
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
        let mut interpreter_opts = sim_opts(SimExecutionPolicy::Interpreter);
        interpreter_opts.solver_mode = rumoca_solver::SimSolverMode::RkLike;
        let model = state_fixture(&interpreter_opts);
        let (_backend, counters, handle) = counting_handle(&model);
        let error = run_test_artifact(model.artifact(), &interpreter_opts, Some(handle))
            .expect_err("interpreter policy plus a handle must be rejected, never executed");
        assert!(
            matches!(
                error.kind(),
                SimError::ExecutionPolicyContradiction {
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
    /// `rumoca_solver::fmi_me::select_execution` and enforced at the
    /// rk45 entry points themselves, so the public bypass around
    /// composition-time withholding in `rumoca-sim` is closed: a direct
    /// caller cannot obtain backend-dependent semantics for one request.
    #[cfg(feature = "solver-rk45")]
    #[test]
    fn rk45_interpreter_policy_with_handle_is_a_typed_rejection() {
        let mut interpreter_opts = sim_opts(SimExecutionPolicy::Interpreter);
        interpreter_opts.solver_mode = rumoca_solver::SimSolverMode::RkLike;
        let model = state_fixture(&interpreter_opts);
        let (_backend, counters, handle) = counting_handle(&model);
        let error = match prepare_test_artifact(model.artifact(), &interpreter_opts, Some(handle)) {
            Ok(_) => {
                panic!("interpreter policy plus a handle must be rejected on the rk-like path too")
            }
            Err(error) => error,
        };
        assert!(
            matches!(
                error.kind(),
                SimError::ExecutionPolicyContradiction {
                    policy: "interpreter"
                }
            ),
            "expected the shared typed policy contradiction, got: {error}"
        );
        assert_eq!(
            counters.total_activity(),
            0,
            "the rejected handle must never be compiled against or called on the rk-like path"
        );

        let (_session_backend, session_counters, session_handle) = counting_handle(&model);
        let session_error = crate::rk45::SimulationSession::from_selected_artifact(
            model.artifact(),
            interpreter_opts.clone(),
            Some(session_handle),
        )
        .err()
        .expect("the session entry point must reject the same contradiction");
        assert!(
            matches!(
                &session_error,
                crate::SimulationDiagnosticError::Solver(message)
                    if message.contains("execution policy 'interpreter'")
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
    /// sites (the rk45 batch path, the stage-timing session build, and
    /// `rk45::SimulationSession::from_selected_artifact`) route through the ONE
    /// shared admission gate asserted here, which withholds — returning
    /// `None` before any backend is built — when `state_scalar_count() == 0`,
    /// so a pure-discrete request never pays Cranelift composition cost it
    /// cannot use. The state-carrying assertion keeps the check non-vacuous,
    /// and the full rk-like zero-state run proves the withheld path still
    /// completes.
    #[cfg(feature = "solver-rk45")]
    #[test]
    fn rk45_zero_state_composition_constructs_no_backend() {
        let mut opts = sim_opts(SimExecutionPolicy::Auto);
        opts.solver_mode = rumoca_solver::SimSolverMode::RkLike;
        let zero_model = zero_state_discrete_fixture(&opts);
        assert_eq!(
            zero_model.state_scalar_count(),
            0,
            "fixture must be zero-state"
        );
        assert!(
            admitted_native_execution_backend(&opts, &zero_model)
                .expect("zero-state admission succeeds")
                .is_none(),
            "the shared admission gate must withhold the backend for a zero-state model \
             under Auto — no backend may even be constructed"
        );
        let state_model = state_fixture(&opts);
        assert!(
            admitted_native_execution_backend(&opts, &state_model)
                .expect("state-bearing admission succeeds")
                .is_some(),
            "the same gate must compose a backend for a state-carrying model under Auto, \
             or the zero-state assertion above is vacuous"
        );
        // Building through the real rk45 composition call site proves the
        // automatically withheld path still completes.
        crate::rk45::SimulationSession::from_selected_artifact(zero_model.artifact(), opts, None)
            .expect("the rk-like zero-state session builds with the backend withheld");
    }

    /// An explicitly supplied execution backend is a component evaluator, not
    /// a numerical integrator. A zero-state component may therefore use it for
    /// discrete/algebraic programs even though the automatic composition gate
    /// correctly constructs no backend. Both rk-like session forms must honor
    /// the supplied handle and release it with ME ownership.
    #[cfg(feature = "solver-rk45")]
    #[test]
    fn rk45_zero_state_force_supplied_handle_is_honored_and_released() {
        let mut opts = sim_opts(SimExecutionPolicy::Auto);
        opts.solver_mode = rumoca_solver::SimSolverMode::RkLike;
        let model = zero_state_discrete_fixture(&opts);
        assert_eq!(model.state_scalar_count(), 0, "fixture must be zero-state");

        let (backend, counters, handle) = counting_handle(&model);
        let result = run_test_artifact(model.artifact(), &opts, Some(handle))
            .expect("the shared ME batch path executes a zero-state model");
        assert!(
            !result.times.is_empty(),
            "the zero-state batch run produced no observations"
        );
        assert!(
            counters.total_succeeded() > 0,
            "the explicitly supplied component evaluator was silently discarded"
        );
        assert_eq!(counters.total_failed(), 0);
        assert_eq!(
            Rc::strong_count(&backend),
            1,
            "the completed zero-state batch run retained its component evaluator"
        );

        let (session_backend, session_counters, session_handle) = counting_handle(&model);
        let mut session = crate::rk45::SimulationSession::from_selected_artifact(
            model.artifact(),
            opts.clone(),
            Some(session_handle),
        )
        .expect("rk-like zero-state session builds with a force-supplied handle");
        session
            .advance_to(opts.t_end)
            .expect("the zero-state session executes through the common component");
        assert!(
            session_counters.total_succeeded() > 0,
            "the zero-state session silently discarded its supplied component evaluator"
        );
        assert_eq!(session_counters.total_failed(), 0);
        drop(session);
        assert_eq!(
            Rc::strong_count(&session_backend),
            1,
            "after the zero-state session is dropped no other owner may retain the backend"
        );
    }

    /// The one public prepared product owns an already initialized session.
    /// Running it performs numerical work without recompiling, and consuming
    /// `run(self)` prevents a second admission or initialization by type.
    #[test]
    fn prepared_run_uses_the_once_initialized_component_without_recompiling() {
        let opts = sim_opts(SimExecutionPolicy::Auto);
        let model = state_fixture(&opts);
        let (_backend, counters, handle) = counting_handle(&model);
        let prepared = prepare_test_artifact(model.artifact(), &opts, Some(handle))
            .expect("native BDF build succeeds");
        let compiles_after_build = counters.compile_counts();
        assert!(
            counters.expression_compiles.get() > 0,
            "the build performed no native expression compiles — the pin would be vacuous"
        );
        assert!(
            counters.assignment_compiles.get() > 0,
            "preparation compiled no exact-assignment schedule — the eager-obligation half of \
             this pin would be vacuous"
        );

        let successes_after_preparation = counters.total_succeeded();
        let assignment_after_preparation = counters.assignment.succeeded.get();
        let result = prepared.run().expect("the prepared run succeeds");
        assert!(
            counters.total_succeeded() > successes_after_preparation,
            "the prepared run performed no native calls"
        );
        assert!(
            counters.assignment.succeeded.get() > assignment_after_preparation,
            "the run did not traverse the compiled exact-assignment schedule"
        );
        assert_eq!(
            counters.compile_counts(),
            compiles_after_build,
            "the run recompiled native programs that preparation already compiled"
        );
        assert_eq!(
            counters.total_failed(),
            0,
            "the prepared native run must not fail selected native calls"
        );
        assert!(
            !result.times.is_empty() && result.times.last() == Some(&opts.t_end),
            "the one-shot prepared run must publish the complete requested grid"
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
        run_test_artifact(model.artifact(), &opts, Some(one_shot_handle))
            .expect("one-shot native BDF run succeeds");
        let (_build_backend, build_counters, build_handle) = counting_handle(&model);
        let _prepared = prepare_test_artifact(model.artifact(), &opts, Some(build_handle))
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
            inner: crate::native_execution::backend(model_a.pure_calls())
                .expect("fixture pure-call table compiles"),
            counters: counters.clone(),
        });
        let handle_a = MeExecutionBackend::new(factory.clone() as Rc<dyn SolveExecutionBackend>);
        let handle_b = MeExecutionBackend::new(factory.clone() as Rc<dyn SolveExecutionBackend>);

        let prepared_a = prepare_test_artifact(model_a.artifact(), &opts, Some(handle_a))
            .expect("first model builds");
        let result_a = prepared_a.run().expect("first model runs");
        let compiles_after_a = counters.compile_counts();
        assert!(
            counters.expression_compiles.get() > 0,
            "the first model performed no native compiles — the pin would be vacuous"
        );

        let prepared_b = prepare_test_artifact(model_b.artifact(), &opts, Some(handle_b))
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
            "cross-model native runs must not fail selected native calls"
        );
    }

    #[test]
    fn preparation_releases_backend_factory_after_sealing_compiled_arms() {
        let opts = sim_opts(SimExecutionPolicy::Auto);
        let model = state_fixture(&opts);
        let (backend, _counters, handle) = counting_handle(&model);
        let prepared = prepare_test_artifact(model.artifact(), &opts, Some(handle))
            .expect("native BDF preparation succeeds");
        assert_eq!(
            Rc::strong_count(&backend),
            1,
            "the runnable session owns compiled obligations, not a backend factory"
        );
        prepared.run().expect("prepared native BDF run succeeds");
    }

    /// The automatic composition gate never constructs a compiled evaluator
    /// for a zero-state model. An explicitly supplied evaluator is nonetheless
    /// honored by the common component for its discrete/algebraic programs and
    /// released when the run ends.
    #[test]
    fn zero_state_force_supplied_backend_is_honored_and_released() {
        let opts = sim_opts(SimExecutionPolicy::Auto);
        let model = zero_state_fixture(&opts);
        assert_eq!(model.state_scalar_count(), 0, "fixture must be zero-state");
        assert!(
            admitted_native_execution_backend(&opts, &model)
                .expect("zero-state admission succeeds")
                .is_none(),
            "a zero-state model must not pay for a native backend it cannot use"
        );
        let (backend, counters, handle) = counting_handle(&model);
        let result = run_test_artifact(model.artifact(), &opts, Some(handle))
            .expect("zero-state run succeeds");
        assert!(
            !result.times.is_empty(),
            "zero-state run produced no samples"
        );
        assert!(
            counters.total_succeeded() > 0,
            "the explicitly supplied component evaluator was silently discarded"
        );
        assert_eq!(counters.total_failed(), 0);
        assert_eq!(
            Rc::strong_count(&backend),
            1,
            "the zero-state path retained its supplied evaluator after completion"
        );
    }
}
