#[cfg(test)]
mod tests;

use std::rc::Rc;

struct CraneliftExpression(rumoca_exec_cranelift::CompiledExpressionRows);

struct CraneliftJacobianExpression(rumoca_exec_cranelift::CompiledJacobianV);

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
    fn call_program_outputs(
        &self,
        program: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
        external_tables: &[rumoca_core::ExternalTableData],
        out: &mut Vec<f64>,
    ) -> Result<bool, String> {
        self.0
            .call_program_outputs(program, y, p, t, external_tables, out)
            .map_err(|error| error.to_string())
    }

    fn call_program_output(
        &self,
        coordinate: (usize, usize),
        y: &[f64],
        p: &[f64],
        t: f64,
        external_tables: &[rumoca_core::ExternalTableData],
    ) -> Result<Option<f64>, String> {
        self.0
            .call_program_output(coordinate, y, p, t, external_tables)
            .map_err(|error| error.to_string())
    }

    fn call_program_outputs_at(
        &self,
        coordinates: &[(usize, usize)],
        inputs: (&[f64], &[f64], f64),
        external_tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<bool, String> {
        self.0
            .call_program_outputs_at(coordinates, inputs, external_tables, out)
            .map_err(|error| error.to_string())
    }

    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        external_tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), String> {
        self.0
            .call_with_external_tables(y, p, t, external_tables, out)
            .map_err(|error| error.to_string())
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
    ) -> Result<(), String> {
        self.0
            .call_with_external_tables(y, p, t, external_tables, out)
            .map_err(|error| error.to_string())
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
    ) -> Result<(), String> {
        self.0
            .call_with_external_tables(y, p, t, seed, external_tables, out)
            .map_err(|error| error.to_string())
    }
}

impl rumoca_solver::CompiledSolveJacobianExpression for CraneliftJacobianExpression {
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
    ) -> Result<bool, String> {
        self.0
            .call_program_outputs(program, inputs, external_tables, out)
            .map(|()| true)
            .map_err(|error| error.to_string())
    }

    fn call_program_output(
        &self,
        coordinate: (usize, usize),
        y: &[f64],
        p: &[f64],
        t: f64,
        seed: &[f64],
        external_tables: &[rumoca_core::ExternalTableData],
    ) -> Result<Option<f64>, String> {
        self.0
            .call_program_output(coordinate, y, p, t, seed, external_tables)
            .map(Some)
            .map_err(|error| error.to_string())
    }

    fn call_program_outputs_at(
        &self,
        coordinates: &[(usize, usize)],
        inputs: rumoca_eval_solve::JacobianEvalInputs<'_>,
        external_tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<bool, String> {
        self.0
            .call_program_outputs_at(coordinates, inputs, external_tables, out)
            .map(|()| true)
            .map_err(|error| error.to_string())
    }

    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        seed: &[f64],
        external_tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), String> {
        self.0
            .call_with_external_tables(y, p, t, seed, external_tables, out)
            .map_err(|error| error.to_string())
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
    ) -> Result<(), String> {
        self.0
            .call(y, p, t, external_tables, out)
            .map_err(|error| error.to_string())
    }
}

impl rumoca_solver::CompiledSolveAssignmentSchedule for CraneliftAssignmentSchedule {
    fn call(
        &self,
        y: &mut [f64],
        p: &[f64],
        t: f64,
        external_tables: &[rumoca_core::ExternalTableData],
    ) -> Result<(), String> {
        self.0
            .call_with_external_tables(y, p, t, external_tables)
            .map_err(|error| error.to_string())
    }
}

impl rumoca_solver::CompiledSolveEventTransaction for CraneliftEventTransaction {
    fn call(&self, input: &[f64], output: &mut [f64]) -> Result<(), String> {
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
            .map_err(|error| error.to_string())
    }
}

struct CraneliftExecutionBackend {
    pure_calls: Option<rumoca_exec_cranelift::CompiledPureCallTable>,
    call_cells: std::cell::RefCell<(Vec<u64>, Vec<u64>)>,
}

impl rumoca_eval_solve::PureCallExecution for CraneliftExecutionBackend {
    fn call(
        &self,
        invocation: rumoca_eval_solve::PureCallInvocation<'_>,
        input: &[f64],
        output: &mut [f64],
    ) -> Result<(), rumoca_eval_solve::EvalSolveError> {
        let table = self.pure_calls.as_ref().ok_or(
            rumoca_eval_solve::EvalSolveError::MissingRuntimeState {
                operation: "native pure-call table",
            },
        )?;
        let mut cells = self.call_cells.borrow_mut();
        let (input_cells, output_cells) = &mut *cells;
        table
            .call_scalar_payload(invocation, input, output, input_cells, output_cells)
            .map_err(|error| rumoca_eval_solve::EvalSolveError::InvalidRow {
                message: error.to_string(),
                span: None,
            })
    }
}

impl rumoca_solver::SolveExecutionBackend for CraneliftExecutionBackend {
    fn pure_call_execution(&self) -> Option<&dyn rumoca_eval_solve::PureCallExecution> {
        self.pure_calls
            .as_ref()
            .map(|_| self as &dyn rumoca_eval_solve::PureCallExecution)
    }

    fn compile_expression(
        &self,
        block: &rumoca_ir_solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn rumoca_solver::CompiledSolveExpression>, String> {
        let compiled = match &self.pure_calls {
            Some(pure_calls) => {
                rumoca_exec_cranelift::compile_expression_scalar_program_block_with_pure_calls(
                    block, pure_calls,
                )
            }
            None => rumoca_exec_cranelift::compile_expression_scalar_program_block(block),
        };
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
            self.pure_calls.as_ref(),
        )
        .map(|compiled| Rc::new(CraneliftExpression(compiled)) as Rc<_>)
        .map_err(|error| error.to_string())
    }

    fn compile_compute_expression(
        &self,
        block: &rumoca_ir_solve::ComputeBlock,
    ) -> Result<Option<Rc<dyn rumoca_solver::CompiledSolveExpression>>, String> {
        rumoca_exec_cranelift::compile_expression_compute_block(block, self.pure_calls.as_ref())
            .map(|compiled| {
                compiled.map(|compiled| Rc::new(CraneliftComputeExpression(compiled)) as Rc<_>)
            })
            .map_err(|error| error.to_string())
    }

    fn compile_compute_jacobian_expression(
        &self,
        block: &rumoca_ir_solve::ComputeBlock,
    ) -> Result<Option<Rc<dyn rumoca_solver::CompiledSolveJacobianExpression>>, String> {
        rumoca_exec_cranelift::compile_jacobian_compute_block(block, self.pure_calls.as_ref())
            .map(|compiled| {
                compiled.map(|compiled| Rc::new(CraneliftComputeJacobian(compiled)) as Rc<_>)
            })
            .map_err(|error| error.to_string())
    }

    fn compile_jacobian_expression(
        &self,
        block: &rumoca_ir_solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn rumoca_solver::CompiledSolveJacobianExpression>, String> {
        let compiled = match &self.pure_calls {
            Some(pure_calls) => {
                rumoca_exec_cranelift::compile_jacobian_scalar_program_block_with_pure_calls(
                    block, pure_calls,
                )
            }
            None => rumoca_exec_cranelift::compile_jacobian_scalar_program_block(block),
        };
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
        let compiled = match &self.pure_calls {
            Some(pure_calls) => {
                rumoca_exec_cranelift::compile_exact_assignment_schedule_with_pure_calls(
                    source, owners, schedule, pure_calls,
                )
            }
            None => {
                rumoca_exec_cranelift::compile_exact_assignment_schedule(source, owners, schedule)
            }
        };
        compiled
            .map(|compiled| Rc::new(CraneliftAssignmentSchedule(compiled)) as Rc<_>)
            .map_err(|error| error.to_string())
    }

    fn compile_torn_assignment_rows(
        &self,
        rows: &[Vec<rumoca_ir_solve::LinearOp>],
        target_y_indices: &[usize],
    ) -> Result<Rc<dyn rumoca_solver::CompiledSolveAssignmentSchedule>, String> {
        let compiled = match &self.pure_calls {
            Some(pure_calls) => rumoca_exec_cranelift::compile_assignment_schedule_with_pure_calls(
                rows,
                target_y_indices,
                pure_calls,
            ),
            None => rumoca_exec_cranelift::compile_assignment_schedule(rows, target_y_indices),
        };
        compiled
            .map(|compiled| Rc::new(CraneliftAssignmentSchedule(compiled)) as Rc<_>)
            .map_err(|error| error.to_string())
    }

    fn compile_event_transaction(
        &self,
        program: &rumoca_ir_solve::EventTransactionProgram,
    ) -> Result<Rc<dyn rumoca_solver::CompiledSolveEventTransaction>, String> {
        self.pure_calls
            .as_ref()
            .cloned()
            .map(|pure_calls| {
                Rc::new(CraneliftEventTransaction {
                    pure_calls,
                    site: program.site().clone(),
                    cells: std::cell::RefCell::new((Vec::new(), Vec::new())),
                }) as Rc<_>
            })
            .ok_or_else(|| "the typed pure-call table is unavailable".to_string())
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
) -> Option<rumoca_solver::fmi_me::MeExecutionBackend> {
    if !opts.execution_policy.allows_native() {
        return None;
    }
    if model.state_scalar_count() == 0 {
        return None;
    }
    Some(rumoca_solver::fmi_me::MeExecutionBackend::new(backend(
        &model.pure_calls,
    )))
}

pub(crate) fn backend(
    table: &rumoca_ir_solve::SolvePureCallTable,
) -> Rc<dyn rumoca_solver::SolveExecutionBackend> {
    let pure_calls = match rumoca_exec_cranelift::compile_pure_call_table(table) {
        Ok(compiled) => Some(compiled),
        Err(error) => {
            tracing::debug!(
                target: "rumoca_sim::native_execution",
                %error,
                "typed pure-call table is unavailable to the native backend"
            );
            None
        }
    };
    Rc::new(CraneliftExecutionBackend {
        pure_calls,
        call_cells: Default::default(),
    })
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
