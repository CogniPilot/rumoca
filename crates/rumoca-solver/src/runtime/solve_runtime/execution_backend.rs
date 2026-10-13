use super::*;

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

pub trait SolveExecutionBackend {
    /// Admit the complete model execution context before preparation or calls.
    /// A rejection is fatal; it must never become an interpreted fallback.
    fn validate_model_context(&self, _model: &solve::SolveModel) -> Result<(), String> {
        Ok(())
    }

    fn compile_target_values(
        &self,
        _plan: &solve_eval::PreparedTargetValuePlan,
        _context: RowEvalContext<'_>,
    ) -> Result<Rc<dyn CompiledSolveTargetValues>, String> {
        Err("prepared target values are not compiled by this backend".into())
    }

    fn pure_call_execution(&self) -> Option<&dyn solve_eval::PureCallExecution> {
        None
    }

    fn compile_expression(
        &self,
        block: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveExpression>, String>;

    /// Prepare an expression used through both whole-block and selected-program
    /// calls. Backends may retain batching when separate entry points would
    /// split a shared owner; the selected call then explicitly declines.
    fn compile_selectable_expression(
        &self,
        block: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveExpression>, String> {
        self.compile_expression(block)
    }

    fn compile_jacobian_expression(
        &self,
        block: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveJacobianExpression>, String>;

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
        source: &solve::ComputeBlock,
        owners: &solve::ContinuousRefreshOwners,
        schedule: &solve::ExactRefreshAssignmentSchedule,
    ) -> Result<Rc<dyn CompiledSolveAssignmentSchedule>, String>;

    /// Compile a causally ordered list of single-output isolator rows whose
    /// outputs write to the paired solver-Y targets, so later rows observe
    /// earlier writes. Torn back-substitution batches through this; the
    /// default declines so existing backends stay untouched.
    fn compile_torn_assignment_rows(
        &self,
        _rows: &[Vec<solve::LinearOp>],
        _target_y_indices: &[usize],
    ) -> Result<Rc<dyn CompiledSolveAssignmentSchedule>, String> {
        Err("torn assignment rows are not compiled by this backend".to_string())
    }

    fn compile_event_transaction(
        &self,
        program: &solve::EventTransactionProgram,
    ) -> Result<Rc<dyn CompiledSolveEventTransaction>, String>;
}
