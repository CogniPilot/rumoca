use std::rc::Rc;

struct CraneliftExpression(rumoca_exec_cranelift::CompiledExpressionRows);

struct CraneliftJacobianExpression(rumoca_exec_cranelift::CompiledJacobianV);

struct CraneliftAssignmentSchedule(rumoca_exec_cranelift::CompiledAssignmentSchedule);

struct CraneliftEventTransaction {
    pure_calls: rumoca_exec_cranelift::CompiledPureCallTable,
    site: rumoca_ir_solve::SolvePureCallSite,
    cells: std::cell::RefCell<(Vec<u64>, Vec<u64>)>,
}

impl rumoca_solver::CompiledSolveExpression for CraneliftExpression {
    fn call(&self, y: &[f64], p: &[f64], t: f64, out: &mut [f64]) -> Result<(), String> {
        self.0.call(y, p, t, out).map_err(|error| error.to_string())
    }
}

impl rumoca_solver::CompiledSolveJacobianExpression for CraneliftJacobianExpression {
    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        seed: &[f64],
        out: &mut [f64],
    ) -> Result<(), String> {
        self.0
            .call(y, p, t, seed, out)
            .map_err(|error| error.to_string())
    }
}

impl rumoca_solver::CompiledSolveAssignmentSchedule for CraneliftAssignmentSchedule {
    fn call(&self, y: &mut [f64], p: &[f64], t: f64) -> Result<(), String> {
        self.0.call(y, p, t).map_err(|error| error.to_string())
    }
}

impl rumoca_solver::CompiledSolveEventTransaction for CraneliftEventTransaction {
    fn call(&self, input: &[f64], output: &mut [f64]) -> Result<(), String> {
        let mut cells = self.cells.borrow_mut();
        let (input_cells, output_cells) = &mut *cells;
        self.pure_calls
            .call_scalar_payload(&self.site, input, output, input_cells, output_cells)
            .map_err(|error| error.to_string())
    }
}

struct CraneliftExecutionBackend {
    pure_calls: rumoca_exec_cranelift::CompiledPureCallTable,
}

impl rumoca_solver::SolveExecutionBackend for CraneliftExecutionBackend {
    fn compile_expression(
        &self,
        block: &rumoca_ir_solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn rumoca_solver::CompiledSolveExpression>, String> {
        let compiled =
            rumoca_exec_cranelift::compile_expression_scalar_program_block_with_pure_calls(
                block,
                &self.pure_calls,
            );
        compiled
            .map(|compiled| Rc::new(CraneliftExpression(compiled)) as Rc<_>)
            .map_err(|error| error.to_string())
    }

    fn compile_jacobian_expression(
        &self,
        block: &rumoca_ir_solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn rumoca_solver::CompiledSolveJacobianExpression>, String> {
        let compiled = rumoca_exec_cranelift::compile_jacobian_scalar_program_block_with_pure_calls(
            block,
            &self.pure_calls,
        );
        compiled
            .map(|compiled| Rc::new(CraneliftJacobianExpression(compiled)) as Rc<_>)
            .map_err(|error| error.to_string())
    }

    fn compile_assignment_schedule(
        &self,
        execution: &rumoca_ir_solve::ExactRefreshAssignmentExecution<'_>,
    ) -> Result<Rc<dyn rumoca_solver::CompiledSolveAssignmentSchedule>, String> {
        let compiled = rumoca_exec_cranelift::compile_exact_refresh_assignment(execution);
        compiled
            .map(|compiled| Rc::new(CraneliftAssignmentSchedule(compiled)) as Rc<_>)
            .map_err(|error| error.to_string())
    }

    fn compile_event_transaction(
        &self,
        program: &rumoca_ir_solve::EventTransactionProgram,
    ) -> Result<Rc<dyn rumoca_solver::CompiledSolveEventTransaction>, String> {
        Ok(Rc::new(CraneliftEventTransaction {
            pure_calls: self.pure_calls.clone(),
            site: program.site().clone(),
            cells: std::cell::RefCell::new((Vec::new(), Vec::new())),
        }) as Rc<_>)
    }
}

/// The single sim-side admission gate for compiled native execution
/// (SPEC_0038 §Internal Solver Boundary, SPEC_0041 §4).
///
/// Every concrete numerical plugin — RK45 and BDF alike —
/// composes its opaque `MeExecutionBackend` handle through this one helper, so
/// the admission rules cannot drift between paths:
/// - `SimExecutionPolicy::Interpreter` withholds the handle, which is what
///   makes the interpreter side of the backend differential oracle selectable
///   from the request itself rather than from an ambient process setting;
/// - a zero-state (pure-discrete) model withholds it too, BEFORE any backend
///   is built: neither host's zero-state session instantiates an integrator
///   component, so constructing a backend would pay compilation cost for
///   compiled code that is discarded unused.
pub(crate) fn admitted_native_execution_backend(
    opts: &rumoca_solver::SimOptions,
    model: &rumoca_ir_solve::SolveModel,
) -> Result<Option<rumoca_solver::fmi_me::MeExecutionBackend>, rumoca_solver::RuntimeSolveError> {
    if !opts.execution_policy.allows_native() {
        return Ok(None);
    }
    if model.state_scalar_count() == 0 {
        return Ok(None);
    }
    Ok(Some(rumoca_solver::fmi_me::MeExecutionBackend::new(
        backend(model.pure_calls())?,
    )))
}

pub(crate) fn backend(
    table: &rumoca_ir_solve::SolvePureCallTable,
) -> Result<Rc<dyn rumoca_solver::SolveExecutionBackend>, rumoca_solver::RuntimeSolveError> {
    let pure_calls = rumoca_exec_cranelift::compile_pure_call_table(table).map_err(|error| {
        rumoca_solver::RuntimeSolveError::NativeExecution {
            stage: rumoca_solver::NativeExecutionStage::Compile,
            owner: rumoca_solver::NativeExecutionOwner::PureCallTable,
            reason: error.to_string(),
        }
    })?;
    Ok(Rc::new(CraneliftExecutionBackend { pure_calls }))
}
