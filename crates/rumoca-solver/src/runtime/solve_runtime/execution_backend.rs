use super::*;

/// Solver-owned demand for the optional finite full-state Jacobian product.
/// Not requesting a product is distinct from a backend declining it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ModelStateJacobianRequest {
    NotRequested,
    FiniteAssembly,
}

/// One source-bound dense state-Jacobian callable in canonical solver order.
/// The implementation retains the immutable source and all executable owners.
pub trait CompiledSolveModelStateJacobian {
    fn call(
        &self,
        state: &[f64],
        parameters: &[f64],
        time: f64,
        out: &mut [f64],
    ) -> Result<(), crate::RuntimeSolveError>;
}

/// Unsupported model capability is distinct from a preparation failure.
/// Only `Unsupported` permits the existing directional implementation.
#[derive(Clone)]
pub enum ModelStateJacobianPreparation {
    Compiled(Rc<dyn CompiledSolveModelStateJacobian>),
    Unsupported { reason: String },
}

/// Complete application of an issued colored Jacobian at fresh coordinates.
pub trait CompiledSolveProjectionJacobian {
    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        external_tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), crate::RuntimeSolveError>;
}

/// Backend-neutral callable for a causally ordered set of exact assignments.
/// Program outputs write directly to the flattened solver-Y target list, so
/// subsequent programs observe earlier assignments while multiple outputs of
/// one source program commit together.
pub trait CompiledSolveAssignmentSchedule {
    fn call(
        &self,
        y: &mut [f64],
        p: &[f64],
        t: f64,
        external_tables: &[rumoca_core::ExternalTableData],
    ) -> Result<(), crate::RuntimeSolveError>;
}

/// Backend-neutral callable for one checked aggregate event transaction.
pub trait CompiledSolveEventTransaction {
    fn call(&self, input: &[f64], output: &mut [f64]) -> Result<(), crate::RuntimeSolveError>;
}

/// Optional execution adapter injected by a concrete simulation backend.
/// Private checked target tuple execution. No solver-Y publication occurs.
pub trait CompiledSolveTargetValues {
    fn call(
        &self,
        selected: usize,
        y: &[f64],
        p: &[f64],
        time: f64,
        context: RowEvalContext<'_>,
    ) -> Result<f64, crate::RuntimeSolveError>;
}

/// Compiled numeric results and private source-bound event observations.
/// Both concrete engines must implement message access; ordinary expressions
/// cannot substitute a predicate-only implementation of this product.
/// ```
/// use rumoca_solver::{CompiledSolveExpression, CompiledSolveEventObservation, RuntimeSolveError};
/// use std::rc::Rc;
/// let _keep_numeric: fn(Rc<dyn CompiledSolveExpression>) -> Rc<dyn CompiledSolveExpression> = |code| code;
/// struct NoFailedRecords;
/// impl CompiledSolveEventObservation for NoFailedRecords {
///     fn call(&self, _: &[f64], _: &[f64], _: f64,
///         _: &[rumoca_core::ExternalTableData], _: &mut [f64])
///         -> Result<(), RuntimeSolveError> { Ok(()) }
///     fn observed_message(&self, _: &rumoca_ir_solve::SolveEventAction) -> Option<String> { None }
/// }
/// let _observed: Rc<dyn CompiledSolveEventObservation> = Rc::new(NoFailedRecords);
/// ```
///
/// ```compile_fail
/// use rumoca_solver::{CompiledSolveExpression, CompiledSolveEventObservation};
/// use std::rc::Rc;
/// fn substitute_numeric(code: Rc<dyn CompiledSolveExpression>)
///     -> Rc<dyn CompiledSolveEventObservation> { code }
/// ```
///
/// ```compile_fail
/// use rumoca_solver::{CompiledSolveEventObservation, RuntimeSolveError};
/// struct PredicateOnly;
/// impl CompiledSolveEventObservation for PredicateOnly {
///     fn call(&self, _: &[f64], _: &[f64], _: f64,
///         _: &[rumoca_core::ExternalTableData], _: &mut [f64])
///         -> Result<(), RuntimeSolveError> { Ok(()) }
/// }
/// ```
pub trait CompiledSolveEventObservation {
    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        time: f64,
        external_tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), crate::RuntimeSolveError>;

    /// None means this successful invocation produced no matching failed record.
    fn observed_message(&self, action: &solve::SolveEventAction) -> Option<String>;
}

pub trait SolveExecutionBackend {
    /// Prepare one model product from the runtime's exact immutable owner.
    /// An error is fatal; it must not be converted to an unsupported capability.
    fn compile_model_state_jacobian(
        &self,
        _source: solve::SolveExecutionSource,
    ) -> Result<ModelStateJacobianPreparation, String> {
        Ok(ModelStateJacobianPreparation::Unsupported {
            reason: "this execution backend has no prepared model state Jacobian".into(),
        })
    }

    /// Admit the complete model execution context before preparation or calls.
    /// A rejection is fatal; it must never become an interpreted fallback.
    fn validate_model_context(&self, _model: &solve::SolveModel) -> Result<(), String> {
        Ok(())
    }

    /// Every owner of one runtime construction has been compiled. A backend
    /// that defers emission builds its code for all of them here, once.
    fn finish_preparation(&self) {}

    fn compile_target_values(
        &self,
        plan: &solve_eval::PreparedTargetValuePlan,
        context: RowEvalContext<'_>,
    ) -> Result<Rc<dyn CompiledSolveTargetValues>, String> {
        compiled_targets::compile(self, plan, context)
    }

    fn pure_call_execution(&self) -> Option<&dyn solve_eval::PureCallExecution> {
        None
    }

    fn compile_event_observation(
        &self,
        _program: solve::RetainedEventProgram,
    ) -> Result<Rc<dyn CompiledSolveEventObservation>, String> {
        Err("checked event observations are not compiled by this backend".into())
    }

    fn compile_expression(
        &self,
        block: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveExpression>, CompileRefusal>;

    /// Prepare an expression used through both whole-block and selected-program
    /// calls. Backends may retain batching when separate entry points would
    /// split a shared owner; the selected call then explicitly declines.
    fn compile_selectable_expression(
        &self,
        block: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveExpression>, CompileRefusal> {
        self.compile_expression(block)
    }

    fn compile_jacobian_expression(
        &self,
        _block: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveJacobianExpression>, CompileRefusal> {
        Err("Jacobian expressions are not compiled by this backend".into())
    }

    /// [`Self::compile_compute_expression`] for a directional (JVP) block.
    fn compile_compute_jacobian_expression(
        &self,
        _block: &solve::ComputeBlock,
    ) -> Result<Option<Rc<dyn CompiledSolveJacobianExpression>>, String> {
        Ok(None)
    }

    /// Prepare a whole compute block for whole-block calls with its affine
    /// tensor nodes executed natively over their compact domains (SPEC_0032
    /// §4). `Ok(None)` means the backend has no compact form for this block,
    /// and the caller compiles the block's scalar view instead.
    fn compile_compute_expression(
        &self,
        _block: &solve::ComputeBlock,
    ) -> Result<Option<Rc<dyn CompiledSolveExpression>>, String> {
        Ok(None)
    }

    fn compile_assignment_schedule(
        &self,
        _source: &solve::ComputeBlock,
        _owners: &solve::ContinuousRefreshOwners,
        _schedule: &solve::ExactRefreshAssignmentSchedule,
    ) -> Result<Rc<dyn CompiledSolveAssignmentSchedule>, String> {
        Err("assignment schedules are not compiled by this backend".into())
    }

    /// Compile the causally ordered single-output isolator rows of an issued
    /// torn sweep, whose outputs write to the paired solver-Y targets, so
    /// later rows observe earlier writes. Torn back-substitution batches
    /// through this; the default declines so existing backends stay untouched.
    fn compile_torn_assignment_rows(
        &self,
        _sweep: &solve_eval::TornSweepComposite,
    ) -> Result<Rc<dyn CompiledSolveAssignmentSchedule>, String> {
        Err("torn assignment rows are not compiled by this backend".to_string())
    }

    fn compile_event_transaction(
        &self,
        _program: &solve::EventTransactionProgram,
    ) -> Result<Rc<dyn CompiledSolveEventTransaction>, String> {
        Err("event transactions are not compiled by this backend".into())
    }
}
