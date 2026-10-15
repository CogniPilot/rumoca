//! Portable scalar row execution behind the existing ME component boundary.

use super::{assignments, profile};

use rumoca_ir_solve::{ScalarProgramBlock, VarLayout};
use std::{cell::RefCell, rc::Rc};

mod selected;
#[cfg(test)]
mod tests;

pub(super) struct WasmExpression {
    pub(super) layout: Rc<VarLayout>,
    pub(super) source: ScalarProgramBlock,
    // Each source program owns its complete private output tuple.
    selected: Vec<Rc<selected::Program>>,
    scratch: RefCell<Vec<f64>>,
    directional: bool,
}

impl WasmExpression {
    fn compile(
        source: &ScalarProgramBlock,
        layout: &VarLayout,
        calls: &rumoca_ir_solve::SolvePureCallTable,
    ) -> Result<Self, String> {
        Self::compile_with_seed(source, layout, calls, None)
    }

    pub(super) fn compile_with_seed(
        source: &ScalarProgramBlock,
        layout: &VarLayout,
        calls: &rumoca_ir_solve::SolvePureCallTable,
        seed_len: Option<usize>,
    ) -> Result<Self, String> {
        let selected = source
            .programs()
            .iter()
            .enumerate()
            .map(|(index, ops)| {
                let span = source
                    .program_span(index)
                    .ok_or("WASM source span is absent")?;
                let block = ScalarProgramBlock::with_program_spans(vec![ops.clone()], vec![span])
                    .map_err(|error| error.to_string())?;
                let code = match seed_len {
                    Some(count) => rumoca_exec_wasm::compile_private_directional_program_wasm(
                        &block, layout, calls, count,
                    ),
                    None => rumoca_exec_wasm::compile_private_program_wasm(&block, layout, calls),
                }
                .map_err(|error| error.to_string())?;
                let count = usize::try_from(code.scratch_bytes())
                    .map_err(|_| "WASM expression scratch exceeds usize")?
                    .div_ceil(8);
                Ok(Rc::new((code, RefCell::new(vec![0.0; count]))))
            })
            .collect::<Result<Vec<_>, String>>()?;
        Ok(Self {
            layout: Rc::new(layout.clone()),
            source: source.clone(),
            selected,
            scratch: RefCell::new(vec![0.0; source.output_count()]),
            directional: seed_len.is_some(),
        })
    }

    fn selected_values(
        &self,
        program: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut Vec<f64>,
    ) -> Result<(), rumoca_solver::RuntimeSolveError> {
        self.values_with_seed(
            program,
            rumoca_eval_solve::JacobianEvalInputs { y, p, t, seed: &[] },
            tables,
            out,
        )
    }

    pub(super) fn values_with_seed(
        &self,
        program: usize,
        inputs: rumoca_eval_solve::JacobianEvalInputs<'_>,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut Vec<f64>,
    ) -> Result<(), rumoca_solver::RuntimeSolveError> {
        let program = self.selected.get(program).ok_or_else(|| {
            rumoca_solver::RuntimeSolveError::solve_ir(format!(
                "WASM ME source program {program} is outside the prepared expression"
            ))
        })?;
        let value = selected::invoke(program, &self.layout, inputs, tables, self.directional)?;
        let count = program.0.output_count();
        out.try_reserve(count.saturating_sub(out.len()))
            .map_err(|error| error.to_string())?;
        out.clear();
        out.extend_from_slice(&value[..count]);
        Ok(())
    }

    pub(super) fn call_with_seed(
        &self,
        inputs: rumoca_eval_solve::JacobianEvalInputs<'_>,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), rumoca_solver::RuntimeSolveError> {
        profile::validate_inputs(&self.layout, inputs.y.len(), inputs.p.len(), tables.len())?;
        if out.len() != self.source.output_count() {
            return Err("WASM ME output length differs from the original block".into());
        }
        let mut scratch = self.scratch.borrow_mut();
        scratch.fill(0.0);
        let mut values = Vec::new();
        let mut ordinal = 0;
        for program in 0..self.selected.len() {
            self.values_with_seed(program, inputs, tables, &mut values)?;
            for value in &values {
                scratch[self.source.output_indices()[ordinal]] = *value;
                ordinal += 1;
            }
        }
        out.copy_from_slice(&scratch);
        Ok(())
    }
}

impl rumoca_solver::CompiledSolveExpression for WasmExpression {
    fn bind_program(
        &self,
        source: &ScalarProgramBlock,
        program: usize,
    ) -> Result<Option<Rc<dyn rumoca_solver::CompiledSolveExpression>>, String> {
        self.bind_numeric(source, program).map(Some)
    }

    fn call_program_outputs(
        &self,
        program: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut Vec<f64>,
    ) -> Result<bool, rumoca_solver::RuntimeSolveError> {
        self.selected_values(program, y, p, t, tables, out)?;
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
        let code = self
            .selected
            .get(coordinate.0)
            .ok_or("WASM source program is absent")?;
        if coordinate.1 >= code.0.output_count() {
            return Err("WASM source output offset exceeds its original program".into());
        }
        let mut values = Vec::new();
        self.selected_values(coordinate.0, y, p, t, tables, &mut values)?;
        Ok(Some(values[coordinate.1]))
    }

    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), rumoca_solver::RuntimeSolveError> {
        self.call_with_seed(
            rumoca_eval_solve::JacobianEvalInputs { y, p, t, seed: &[] },
            tables,
            out,
        )
    }
}

struct WasmExecutionBackend {
    layout: VarLayout,
    calls: rumoca_ir_solve::SolvePureCallTable,
}

impl rumoca_solver::SolveExecutionBackend for WasmExecutionBackend {
    fn validate_model_context(&self, model: &rumoca_ir_solve::SolveModel) -> Result<(), String> {
        if !self.layout.same_execution_layout(&model.problem.layout) {
            return Err("portable kernel layout differs from the model execution layout".into());
        }
        if !self.calls.shares_table_owner(&model.pure_calls) {
            return Err("portable kernels belong to a different canonical call table".into());
        }
        Ok(())
    }

    fn compile_event_observation(
        &self,
        program: rumoca_ir_solve::RetainedEventProgram,
    ) -> Result<Rc<dyn rumoca_solver::CompiledSolveEventObservation>, String> {
        super::observations::WasmObservations::compile(program, &self.layout, &self.calls)
            .map(|compiled| Rc::new(compiled) as Rc<_>)
    }

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
        WasmExpression::compile(block, &self.layout, &self.calls)
            .map(|compiled| Rc::new(compiled) as Rc<_>)
    }
    fn compile_jacobian_expression(
        &self,
        block: &ScalarProgramBlock,
    ) -> Result<Rc<dyn rumoca_solver::CompiledSolveJacobianExpression>, String> {
        super::jacobians::WasmJacobian::compile(block, &self.layout, &self.calls)
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
        program: &rumoca_ir_solve::EventTransactionProgram,
    ) -> Result<Rc<dyn rumoca_solver::CompiledSolveEventTransaction>, String> {
        super::transactions::WasmEventTransaction::compile(program, &self.calls)
            .map(|compiled| Rc::new(compiled) as Rc<_>)
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
