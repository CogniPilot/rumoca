//! Portable scalar row execution behind the existing ME component boundary.

use super::{assignments, profile};

use rumoca_exec_wasm::CompiledExpressionRowsWasm;
use rumoca_ir_solve::{ScalarProgramBlock, VarLayout};
use std::{cell::RefCell, rc::Rc};

#[cfg(test)]
mod tests;

struct WasmExpression {
    layout: VarLayout,
    source: ScalarProgramBlock,
    full: CompiledExpressionRowsWasm,
    selected: Vec<CompiledExpressionRowsWasm>,
    scratch: RefCell<Vec<f64>>,
}

impl WasmExpression {
    fn compile(source: &ScalarProgramBlock, layout: &VarLayout) -> Result<Self, String> {
        let profiles = (0..source.row_count())
            .map(|program| profile::single_program(source, program, layout))
            .collect::<Result<Vec<_>, _>>()?;
        let selected = profiles
            .iter()
            .map(|block| {
                rumoca_exec_wasm::compile_expression_scalar_program_block_wasm(block, layout)
                    .map_err(|error| error.to_string())
            })
            .collect::<Result<Vec<_>, _>>()?;
        let local = ScalarProgramBlock::with_program_spans(
            source.programs().to_vec(),
            source.program_spans().to_vec(),
        )
        .map_err(|error| error.to_string())?;
        let full = rumoca_exec_wasm::compile_expression_scalar_program_block_wasm(&local, layout)
            .map_err(|error| error.to_string())?;
        tracing::debug!(target: "rumoca_sim::native_execution", programs=source.row_count(), "portable ME scalar preparation complete");
        Ok(Self {
            layout: layout.clone(),
            source: source.clone(),
            full,
            selected,
            scratch: RefCell::new(vec![0.0; source.stored_output_count()]),
        })
    }

    fn selected_value(
        &self,
        program: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
        tables: &[rumoca_core::ExternalTableData],
    ) -> Result<f64, rumoca_solver::RuntimeSolveError> {
        let compiled = self.selected.get(program).ok_or_else(|| {
            rumoca_solver::RuntimeSolveError::solve_ir(format!(
                "WASM ME source program {program} is outside the prepared expression"
            ))
        })?;
        profile::validate_inputs(&self.layout, y.len(), p.len(), tables.len())?;
        let mut value = [0.0];
        compiled
            .call(y, p, t, &mut value)
            .map_err(rumoca_solver::runtime::native_wasm_error::execution_error)?;
        Ok(value[0])
    }
}

impl rumoca_solver::CompiledSolveExpression for WasmExpression {
    fn call_program_outputs(
        &self,
        program: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut Vec<f64>,
    ) -> Result<bool, rumoca_solver::RuntimeSolveError> {
        let value = self.selected_value(program, y, p, t, tables)?;
        out.try_reserve(1_usize.saturating_sub(out.len()))
            .map_err(|error| error.to_string())?;
        out.clear();
        out.push(value);
        Ok(true)
    }

    fn call_program_output(
        &self,
        coordinate: (usize, usize),
        y: &[f64],
        p: &[f64],
        t: f64,
        tables: &[rumoca_core::ExternalTableData],
    ) -> Result<Option<f64>, rumoca_solver::RuntimeSolveError> {
        if coordinate.1 != 0 {
            return Err("WASM ME scalar source output offset must be zero".into());
        }
        self.selected_value(coordinate.0, y, p, t, tables).map(Some)
    }

    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), rumoca_solver::RuntimeSolveError> {
        profile::validate_inputs(&self.layout, y.len(), p.len(), tables.len())?;
        if out.len() != self.source.output_count() {
            return Err("WASM ME output length differs from the original block".into());
        }
        let mut scratch = self.scratch.borrow_mut();
        self.full
            .call(y, p, t, &mut scratch)
            .map_err(rumoca_solver::runtime::native_wasm_error::execution_error)?;
        out.fill(0.0);
        for (&index, &value) in self.source.output_indices().iter().zip(scratch.iter()) {
            out[index] = value;
        }
        Ok(())
    }
}

struct WasmExecutionBackend {
    layout: VarLayout,
    calls: rumoca_ir_solve::SolvePureCallTable,
}

impl rumoca_solver::SolveExecutionBackend for WasmExecutionBackend {
    fn compile_target_values(
        &self,
        plan: &rumoca_eval_solve::PreparedTargetValuePlan,
        context: rumoca_eval_solve::RowEvalContext<'_>,
    ) -> Result<Rc<dyn rumoca_solver::CompiledSolveTargetValues>, String> {
        super::target_values::WasmTargetValues::compile(plan, &self.layout, &self.calls, context)
            .map(|compiled| Rc::new(compiled) as Rc<_>)
    }

    fn compile_expression(
        &self,
        block: &ScalarProgramBlock,
    ) -> Result<Rc<dyn rumoca_solver::CompiledSolveExpression>, String> {
        WasmExpression::compile(block, &self.layout).map(|compiled| Rc::new(compiled) as Rc<_>)
    }
    fn compile_jacobian_expression(
        &self,
        block: &ScalarProgramBlock,
    ) -> Result<Rc<dyn rumoca_solver::CompiledSolveJacobianExpression>, String> {
        super::jacobians::WasmJacobian::compile(block, &self.layout)
            .map(|code| Rc::new(code) as Rc<_>)
    }
    fn compile_assignment_schedule(
        &self,
        source: &rumoca_ir_solve::ComputeBlock,
        owners: &rumoca_ir_solve::ContinuousRefreshOwners,
        schedule: &rumoca_ir_solve::ExactRefreshAssignmentSchedule,
    ) -> Result<Rc<dyn rumoca_solver::CompiledSolveAssignmentSchedule>, String> {
        assignments::WasmAssignments::compile(source, owners, schedule, &self.layout, &self.calls)
            .map(|compiled| Rc::new(compiled) as Rc<_>)
    }
    fn compile_event_transaction(
        &self,
        _: &rumoca_ir_solve::EventTransactionProgram,
    ) -> Result<Rc<dyn rumoca_solver::CompiledSolveEventTransaction>, String> {
        Err("WASM ME event transaction execution is not admitted".into())
    }
}

pub(crate) fn execution_receipt(
    opts: &rumoca_solver::SimOptions,
    model: &rumoca_ir_solve::SolveModel,
) -> rumoca_solver::SimExecutionReceipt {
    profile::execution_receipt(
        opts.execution_policy,
        model.state_scalar_count(),
        model.external_tables.len(),
    )
}

pub(crate) fn admitted_native_execution_backend(
    opts: &rumoca_solver::SimOptions,
    model: &rumoca_ir_solve::SolveModel,
) -> Result<Option<rumoca_solver::fmi_me::MeExecutionBackend>, crate::SimulationDiagnosticError> {
    if !execution_receipt(opts, model).is_compiled() {
        return Ok(None);
    }
    Ok(Some(rumoca_solver::fmi_me::MeExecutionBackend::new(
        Rc::new(WasmExecutionBackend {
            layout: model.problem.layout.clone(),
            calls: model.pure_calls.clone(),
        }),
    )))
}
