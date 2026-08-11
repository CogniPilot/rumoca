use std::rc::Rc;

struct CraneliftExpression(rumoca_exec_cranelift::CompiledExpressionRows);

struct CraneliftJacobianExpression(rumoca_exec_cranelift::CompiledJacobianV);

struct CraneliftAssignmentSchedule(rumoca_exec_cranelift::CompiledAssignmentSchedule);

impl rumoca_solver::CompiledSolveExpression for CraneliftExpression {
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

impl rumoca_solver::CompiledSolveJacobianExpression for CraneliftJacobianExpression {
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
        programs: &[Vec<rumoca_ir_solve::LinearOp>],
        target_y_indices: &[usize],
    ) -> Result<Rc<dyn rumoca_solver::CompiledSolveAssignmentSchedule>, String> {
        let compiled = match &self.pure_calls {
            Some(pure_calls) => rumoca_exec_cranelift::compile_assignment_schedule_with_pure_calls(
                programs,
                target_y_indices,
                pure_calls,
            ),
            None => rumoca_exec_cranelift::compile_assignment_schedule(programs, target_y_indices),
        };
        compiled
            .map(|compiled| Rc::new(CraneliftAssignmentSchedule(compiled)) as Rc<_>)
            .map_err(|error| error.to_string())
    }
}

pub(crate) fn backend(
    table: &rumoca_ir_solve::SolvePureCallTable,
) -> Rc<dyn rumoca_solver::SolveExecutionBackend> {
    let pure_calls = match rumoca_exec_cranelift::compile_pure_call_table(table) {
        Ok(compiled) => Some(compiled),
        Err(error) => {
            if std::env::var_os("RUMOCA_PROFILE_COMPILED").is_some() {
                eprintln!("rumoca-compiled-profile label=typed-pure-call-table error={error}");
            }
            None
        }
    };
    Rc::new(CraneliftExecutionBackend { pure_calls })
}
