#[cfg(test)]
mod tests;

use std::rc::Rc;

fn compiled_execution_error(
    error: rumoca_exec_cranelift::CompileError,
) -> rumoca_solver::RuntimeSolveError {
    pure_call_execution_error(error).into()
}

fn pure_call_execution_error(
    error: rumoca_exec_cranelift::CompileError,
) -> rumoca_eval_solve::EvalSolveError {
    use rumoca_exec_cranelift::{CompileError, NativeSourceFault};
    match error {
        CompileError::MessageFormatting(error) => error,
        CompileError::SourceOperation {
            kind: NativeSourceFault::TensorIndex,
            message,
            span,
        } => rumoca_eval_solve::EvalSolveError::NativeSourceIndexFault { message, span },
        CompileError::SourceOperation {
            kind: NativeSourceFault::ForeignBody,
            message,
            span,
        } => rumoca_eval_solve::EvalSolveError::SourceOperationFault { message, span },
        CompileError::SourceOperation {
            kind: NativeSourceFault::ExternalTable,
            message,
            span,
        } => rumoca_eval_solve::EvalSolveError::SourceOperationFault { message, span },
        CompileError::SourceOperation { message, span, .. } => {
            rumoca_eval_solve::EvalSolveError::InvalidRow { message, span }
        }
        error => rumoca_eval_solve::EvalSolveError::CompiledExecution {
            message: error.to_string(),
            span: None,
        },
    }
}

struct CraneliftObservedExpression(rumoca_exec_cranelift::CompiledEventObservation);

impl rumoca_solver::CompiledSolveEventObservation for CraneliftObservedExpression {
    fn observed_message(&self, action: &rumoca_ir_solve::SolveEventAction) -> Option<String> {
        self.0.message_for_action(action)
    }
    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), rumoca_solver::RuntimeSolveError> {
        self.0
            .call(y, p, t, tables, out)
            .map_err(compiled_execution_error)
    }
}

struct CraneliftExpression(rumoca_exec_cranelift::CompiledExpressionRows);

struct CraneliftJacobianExpression(rumoca_exec_cranelift::CompiledJacobianV);

struct CraneliftProgramExpression(rumoca_exec_cranelift::CompiledExpressionProgram);

struct CraneliftProgramJacobian(rumoca_exec_cranelift::CompiledJacobianProgram);

struct CraneliftComputeExpression(rumoca_exec_cranelift::CompiledComputeExpression);

struct CraneliftComputeJacobian(rumoca_exec_cranelift::CompiledComputeJacobian);

struct CraneliftProjectionJacobian(rumoca_exec_cranelift::CompiledProjectionJacobian);

struct CraneliftAssignmentSchedule(rumoca_exec_cranelift::CompiledAssignmentSchedule);

struct CraneliftEventTransaction {
    pure_calls: rumoca_exec_cranelift::CompiledPureCallTable,
    site: rumoca_ir_solve::SolvePureCallSite,
    cells: std::cell::RefCell<(Vec<u64>, Vec<u64>)>,
}

impl rumoca_solver::CompiledSolveExpression for CraneliftExpression {
    fn bind_program(
        &self,
        source: &rumoca_ir_solve::ScalarProgramBlock,
        program: usize,
    ) -> Result<Option<Rc<dyn rumoca_solver::CompiledSolveExpression>>, String> {
        self.0
            .bind_program(source, program)
            .map(|entry| entry.map(|entry| Rc::new(CraneliftProgramExpression(entry)) as Rc<_>))
            .map_err(|error| error.to_string())
    }
    fn call_program_outputs(
        &self,
        program: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
        external_tables: &[rumoca_core::ExternalTableData],
        out: &mut Vec<f64>,
    ) -> Result<bool, rumoca_solver::RuntimeSolveError> {
        self.0
            .call_program_outputs(program, y, p, t, external_tables, out)
            .map_err(compiled_execution_error)
    }

    fn call_program_output(
        &self,
        coordinate: (usize, usize),
        y: &[f64],
        p: &[f64],
        t: f64,
        external_tables: &[rumoca_core::ExternalTableData],
    ) -> Result<Option<f64>, rumoca_solver::RuntimeSolveError> {
        self.0
            .call_program_output(coordinate, y, p, t, external_tables)
            .map_err(compiled_execution_error)
    }

    fn call_program_outputs_at(
        &self,
        coordinates: &[(usize, usize)],
        inputs: (&[f64], &[f64], f64),
        external_tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<bool, rumoca_solver::RuntimeSolveError> {
        self.0
            .call_program_outputs_at(coordinates, inputs, external_tables, out)
            .map_err(compiled_execution_error)
    }

    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        external_tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), rumoca_solver::RuntimeSolveError> {
        self.0
            .call_with_external_tables(y, p, t, external_tables, out)
            .map_err(compiled_execution_error)
    }
}

impl rumoca_solver::CompiledSolveExpression for CraneliftComputeExpression {
    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        external_tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), rumoca_solver::RuntimeSolveError> {
        self.0
            .call_with_external_tables(y, p, t, external_tables, out)
            .map_err(compiled_execution_error)
    }
}

impl rumoca_solver::CompiledSolveExpression for CraneliftProgramExpression {
    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), rumoca_solver::RuntimeSolveError> {
        self.0
            .call(y, p, t, tables, out)
            .map_err(compiled_execution_error)
    }
}

impl rumoca_solver::CompiledSolveJacobianExpression for CraneliftProgramJacobian {
    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        seed: &[f64],
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), rumoca_solver::RuntimeSolveError> {
        self.0
            .call(
                rumoca_eval_solve::JacobianEvalInputs { y, p, t, seed },
                tables,
                out,
            )
            .map_err(compiled_execution_error)
    }
}

impl rumoca_solver::CompiledSolveJacobianExpression for CraneliftComputeJacobian {
    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        seed: &[f64],
        external_tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), rumoca_solver::RuntimeSolveError> {
        self.0
            .call_with_external_tables(y, p, t, seed, external_tables, out)
            .map_err(compiled_execution_error)
    }
}

impl rumoca_solver::CompiledSolveJacobianExpression for CraneliftJacobianExpression {
    fn bind_program(
        &self,
        source: &rumoca_ir_solve::ScalarProgramBlock,
        program: usize,
    ) -> Result<Option<Rc<dyn rumoca_solver::CompiledSolveJacobianExpression>>, String> {
        self.0
            .bind_program(source, program)
            .map(|entry| Some(Rc::new(CraneliftProgramJacobian(entry)) as Rc<_>))
            .map_err(|error| error.to_string())
    }
    fn prepare_projections(
        &self,
        applications: &[&rumoca_ir_solve::ProjectionJacobianApplication],
    ) -> Result<Vec<Option<Rc<dyn rumoca_solver::CompiledSolveProjectionJacobian>>>, String> {
        self.0
            .prepare_projections(applications)
            .map(|compiled| {
                compiled
                    .into_iter()
                    .map(|compiled| Some(Rc::new(CraneliftProjectionJacobian(compiled)) as Rc<_>))
                    .collect()
            })
            .map_err(|error| error.to_string())
    }
    fn call_program_outputs(
        &self,
        program: usize,
        inputs: rumoca_eval_solve::JacobianEvalInputs<'_>,
        external_tables: &[rumoca_core::ExternalTableData],
        out: &mut Vec<f64>,
    ) -> Result<bool, rumoca_solver::RuntimeSolveError> {
        self.0
            .call_program_outputs(program, inputs, external_tables, out)
            .map(|()| true)
            .map_err(compiled_execution_error)
    }

    fn call_program_output(
        &self,
        coordinate: (usize, usize),
        y: &[f64],
        p: &[f64],
        t: f64,
        seed: &[f64],
        external_tables: &[rumoca_core::ExternalTableData],
    ) -> Result<Option<f64>, rumoca_solver::RuntimeSolveError> {
        self.0
            .call_program_output(coordinate, y, p, t, seed, external_tables)
            .map(Some)
            .map_err(compiled_execution_error)
    }

    fn call_program_outputs_at(
        &self,
        coordinates: &[(usize, usize)],
        inputs: rumoca_eval_solve::JacobianEvalInputs<'_>,
        external_tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<bool, rumoca_solver::RuntimeSolveError> {
        self.0
            .call_program_outputs_at(coordinates, inputs, external_tables, out)
            .map(|()| true)
            .map_err(compiled_execution_error)
    }

    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        seed: &[f64],
        external_tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), rumoca_solver::RuntimeSolveError> {
        self.0
            .call_with_external_tables(y, p, t, seed, external_tables, out)
            .map_err(compiled_execution_error)
    }
}

impl rumoca_solver::CompiledSolveProjectionJacobian for CraneliftProjectionJacobian {
    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        external_tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), rumoca_solver::RuntimeSolveError> {
        self.0
            .call(y, p, t, external_tables, out)
            .map_err(compiled_execution_error)
    }
}

impl rumoca_solver::CompiledSolveAssignmentSchedule for CraneliftAssignmentSchedule {
    fn call(
        &self,
        y: &mut [f64],
        p: &[f64],
        t: f64,
        external_tables: &[rumoca_core::ExternalTableData],
    ) -> Result<(), rumoca_solver::RuntimeSolveError> {
        self.0
            .call_with_external_tables(y, p, t, external_tables)
            .map_err(compiled_execution_error)
    }
}

impl rumoca_solver::CompiledSolveEventTransaction for CraneliftEventTransaction {
    fn call(
        &self,
        input: &[f64],
        output: &mut [f64],
    ) -> Result<(), rumoca_solver::RuntimeSolveError> {
        let mut cells = self.cells.borrow_mut();
        let (input_cells, output_cells) = &mut *cells;
        self.pure_calls
            .call_scalar_payload(
                rumoca_eval_solve::PureCallInvocation::Primal(&self.site),
                input,
                output,
                input_cells,
                output_cells,
            )
            .map_err(compiled_execution_error)
    }
}

struct CraneliftExecutionBackend {
    source_calls: rumoca_ir_solve::SolvePureCallTable,
    pure_calls: rumoca_exec_cranelift::CompiledPureCallTable,
    call_cells: std::cell::RefCell<(Vec<u64>, Vec<u64>)>,
}

impl rumoca_eval_solve::PureCallExecution for CraneliftExecutionBackend {
    fn executes(&self, invocation: rumoca_eval_solve::PureCallInvocation<'_>) -> bool {
        self.pure_calls.executes(invocation)
    }

    fn call(
        &self,
        invocation: rumoca_eval_solve::PureCallInvocation<'_>,
        input: &[f64],
        output: &mut [f64],
    ) -> Result<(), rumoca_eval_solve::EvalSolveError> {
        let table = &self.pure_calls;
        let mut cells = self.call_cells.borrow_mut();
        let (input_cells, output_cells) = &mut *cells;
        table
            .call_scalar_payload(invocation, input, output, input_cells, output_cells)
            .map_err(pure_call_execution_error)
    }
}

impl rumoca_solver::SolveExecutionBackend for CraneliftExecutionBackend {
    fn validate_model_context(&self, model: &rumoca_ir_solve::SolveModel) -> Result<(), String> {
        if !self.source_calls.shares_table_owner(&model.pure_calls) {
            return Err("native helpers belong to a different canonical call table".into());
        }
        Ok(())
    }

    fn pure_call_execution(&self) -> Option<&dyn rumoca_eval_solve::PureCallExecution> {
        Some(self)
    }

    fn compile_event_observation(
        &self,
        program: rumoca_ir_solve::RetainedEventProgram,
    ) -> Result<Rc<dyn rumoca_solver::CompiledSolveEventObservation>, String> {
        rumoca_exec_cranelift::compile_event_observation(program, &self.pure_calls)
            .map(|compiled| Rc::new(CraneliftObservedExpression(compiled)) as Rc<_>)
            .map_err(|error| error.to_string())
    }

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

    fn compile_selectable_expression(
        &self,
        block: &rumoca_ir_solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn rumoca_solver::CompiledSolveExpression>, String> {
        rumoca_exec_cranelift::compile_selectable_expression_scalar_program_block(
            block,
            Some(&self.pure_calls),
        )
        .map(|compiled| Rc::new(CraneliftExpression(compiled)) as Rc<_>)
        .map_err(|error| error.to_string())
    }

    fn compile_compute_expression(
        &self,
        block: &rumoca_ir_solve::ComputeBlock,
    ) -> Result<Option<Rc<dyn rumoca_solver::CompiledSolveExpression>>, String> {
        rumoca_exec_cranelift::compile_expression_compute_block(block, Some(&self.pure_calls))
            .map(|compiled| {
                compiled.map(|compiled| Rc::new(CraneliftComputeExpression(compiled)) as Rc<_>)
            })
            .map_err(|error| error.to_string())
    }

    fn compile_compute_jacobian_expression(
        &self,
        block: &rumoca_ir_solve::ComputeBlock,
    ) -> Result<Option<Rc<dyn rumoca_solver::CompiledSolveJacobianExpression>>, String> {
        rumoca_exec_cranelift::compile_jacobian_compute_block(block, Some(&self.pure_calls))
            .map(|compiled| {
                compiled.map(|compiled| Rc::new(CraneliftComputeJacobian(compiled)) as Rc<_>)
            })
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
        source: &rumoca_ir_solve::ComputeBlock,
        owners: &rumoca_ir_solve::ContinuousRefreshOwners,
        schedule: &rumoca_ir_solve::ExactRefreshAssignmentSchedule,
    ) -> Result<Rc<dyn rumoca_solver::CompiledSolveAssignmentSchedule>, String> {
        let compiled = rumoca_exec_cranelift::compile_exact_assignment_schedule_with_pure_calls(
            source,
            owners,
            schedule,
            &self.pure_calls,
        );
        compiled
            .map(|compiled| Rc::new(CraneliftAssignmentSchedule(compiled)) as Rc<_>)
            .map_err(|error| error.to_string())
    }

    fn compile_torn_assignment_rows(
        &self,
        rows: &[Vec<rumoca_ir_solve::LinearOp>],
        target_y_indices: &[usize],
    ) -> Result<Rc<dyn rumoca_solver::CompiledSolveAssignmentSchedule>, String> {
        let compiled = rumoca_exec_cranelift::compile_assignment_schedule_with_pure_calls(
            rows,
            target_y_indices,
            &self.pure_calls,
        );
        compiled
            .map(|compiled| Rc::new(CraneliftAssignmentSchedule(compiled)) as Rc<_>)
            .map_err(|error| error.to_string())
    }

    fn compile_event_transaction(
        &self,
        program: &rumoca_ir_solve::EventTransactionProgram,
    ) -> Result<Rc<dyn rumoca_solver::CompiledSolveEventTransaction>, String> {
        if !self
            .pure_calls
            .executes(rumoca_eval_solve::PureCallInvocation::Primal(
                program.site(),
            ))
        {
            return Err("required event transaction has no native pure-call helper".into());
        }
        Ok(Rc::new(CraneliftEventTransaction {
            pure_calls: self.pure_calls.clone(),
            site: program.site().clone(),
            cells: std::cell::RefCell::new((Vec::new(), Vec::new())),
        }))
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
/// - NativeRequired also prepares pure-discrete models; the legacy Auto
///   selection retains its recorded zero-state refusal.
pub(crate) fn execution_receipt(
    opts: &rumoca_solver::SimOptions,
    model: &rumoca_ir_solve::SolveModel,
) -> rumoca_solver::SimExecutionReceipt {
    rumoca_solver::SimExecutionReceipt::admission(
        opts.execution_policy,
        model.state_scalar_count(),
        0,
        rumoca_solver::SimExecutionEngine::Cranelift,
    )
}

pub(crate) fn admitted_native_execution_backend(
    opts: &rumoca_solver::SimOptions,
    model: &rumoca_ir_solve::SolveModel,
) -> Result<Option<rumoca_solver::fmi_me::MeExecutionBackend>, crate::SimulationDiagnosticError> {
    if !execution_receipt(opts, model).is_compiled() {
        return Ok(None);
    }
    let prepared = backend(&model.pure_calls).map_err(|message| {
        crate::SimulationDiagnosticError::RuntimePreparation {
            message,
            span: None,
        }
    })?;
    Ok(Some(rumoca_solver::fmi_me::MeExecutionBackend::new(
        prepared,
    )))
}

pub(crate) fn backend(
    table: &rumoca_ir_solve::SolvePureCallTable,
) -> Result<Rc<dyn rumoca_solver::SolveExecutionBackend>, String> {
    let pure_calls =
        rumoca_exec_cranelift::compile_pure_call_table(table).map_err(|error| error.to_string())?;
    Ok(Rc::new(CraneliftExecutionBackend {
        source_calls: table.clone(),
        pure_calls,
        call_cells: Default::default(),
    }))
}

/// How the native backend compiles one compute block for whole-block calls:
/// compact tensor nodes running as loop kernels, and scalar rows compiled one
/// by one (SPEC_0032 §4). A block no loop kernel owns compiles every row of
/// its scalar view.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct NativeComputeInventory {
    pub kernels: usize,
    pub compiled_rows: usize,
}

impl NativeComputeInventory {
    fn of(
        compiled: Option<(usize, usize)>,
        block: &rumoca_ir_solve::ComputeBlock,
    ) -> Result<Self, String> {
        let (kernels, compiled_rows) = match compiled {
            Some(counts) => counts,
            None => (
                0,
                rumoca_eval_solve::to_scalar_program_block(block)
                    .map_err(|error| error.to_string())?
                    .programs()
                    .len(),
            ),
        };
        Ok(Self {
            kernels,
            compiled_rows,
        })
    }
}

/// The native compilation inventory of `block`.
pub fn native_compute_inventory(
    block: &rumoca_ir_solve::ComputeBlock,
) -> Result<NativeComputeInventory, String> {
    let compiled = rumoca_exec_cranelift::compile_expression_compute_block(block, None)
        .map_err(|error| error.to_string())?
        .map(|compiled| (compiled.kernel_count(), compiled.compiled_row_count()));
    NativeComputeInventory::of(compiled, block)
}

/// The native compilation inventories of a problem's initialization residual
/// and of its directional derivative (the tensor JVP the Solve artifacts carry
/// as the initialization Jacobian).
pub fn native_initialization_inventory(
    problem: &rumoca_ir_solve::SolveProblem,
    artifacts: &rumoca_ir_solve::SolveArtifacts,
) -> Result<(NativeComputeInventory, NativeComputeInventory), String> {
    let jacobian = &artifacts.initialization.residual_jacobian_v;
    let directional = rumoca_exec_cranelift::compile_jacobian_compute_block(jacobian, None)
        .map_err(|error| error.to_string())?
        .map(|compiled| (compiled.kernel_count(), compiled.compiled_row_count()));
    Ok((
        native_compute_inventory(problem.initialization.residual())?,
        NativeComputeInventory::of(directional, jacobian)?,
    ))
}
