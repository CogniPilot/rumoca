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
    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        external_tables: &[rumoca_ir_solve::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), String> {
        self.0
            .call_with_external_tables(y, p, t, external_tables, out)
            .map_err(|error| error.to_string())
    }
}

impl rumoca_solver::CompiledSolveJacobianExpression for CraneliftJacobianExpression {
    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        seed: &[f64],
        external_tables: &[rumoca_ir_solve::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), String> {
        self.0
            .call_with_external_tables(y, p, t, seed, external_tables, out)
            .map_err(|error| error.to_string())
    }
}

impl rumoca_solver::CompiledSolveAssignmentSchedule for CraneliftAssignmentSchedule {
    fn call(
        &self,
        y: &mut [f64],
        p: &[f64],
        t: f64,
        external_tables: &[rumoca_ir_solve::ExternalTableData],
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
            .call_scalar_payload(&self.site, input, output, input_cells, output_cells)
            .map_err(|error| error.to_string())
    }
}

struct CraneliftExecutionBackend {
    pure_calls: Option<rumoca_exec_cranelift::CompiledPureCallTable>,
}

impl rumoca_solver::SolveExecutionBackend for CraneliftExecutionBackend {
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

pub(crate) fn backend(
    table: &rumoca_ir_solve::SolvePureCallTable,
) -> Rc<dyn rumoca_solver::SolveExecutionBackend> {
    let pure_calls = match rumoca_exec_cranelift::compile_pure_call_table(table) {
        Ok(compiled) => Some(compiled),
        Err(error) => {
            tracing::debug!(
                target: "rumoca_sim::profile::compiled",
                "rumoca-compiled-profile label=typed-pure-call-table error={error}"
            );
            None
        }
    };
    Rc::new(CraneliftExecutionBackend { pure_calls })
}
