//! Portable scalar row execution behind the existing ME component boundary.

use super::{assignments, profile};

use rumoca_exec_wasm::CompiledExpressionRowsWasm;
use rumoca_ir_solve::{ScalarProgramBlock, VarLayout};
use std::{cell::RefCell, rc::Rc};

struct WasmExpression {
    layout: VarLayout,
    source: ScalarProgramBlock,
    full: Option<CompiledExpressionRowsWasm>,
    selected: Vec<Option<CompiledExpressionRowsWasm>>,
    scratch: RefCell<Vec<f64>>,
}

impl WasmExpression {
    fn compile(
        source: &ScalarProgramBlock,
        layout: &VarLayout,
        selectable: bool,
    ) -> Result<Self, String> {
        let profiles = (0..source.row_count())
            .map(|program| profile::single_program(source, program, layout).ok())
            .collect::<Vec<_>>();
        let complete = profiles.iter().all(Option::is_some);
        if !selectable && !complete {
            return Err("WASM ME whole expression has unsupported original programs".into());
        }
        let full = if complete {
            let local = ScalarProgramBlock::with_program_spans(
                source.programs().to_vec(),
                source.program_spans().to_vec(),
            )
            .map_err(|error| error.to_string())?;
            Some(
                rumoca_exec_wasm::compile_expression_scalar_program_block_wasm(&local, layout)
                    .map_err(|error| error.to_string())?,
            )
        } else {
            None
        };
        let selected = if selectable {
            profiles
                .iter()
                .map(|profile| {
                    profile.as_ref().and_then(|block| {
                        rumoca_exec_wasm::compile_expression_scalar_program_block_wasm(
                            block, layout,
                        )
                        .ok()
                    })
                })
                .collect()
        } else {
            Vec::new()
        };
        let admitted = selected.iter().filter(|program| program.is_some()).count();
        if full.is_none() && admitted == 0 {
            return Err("WASM ME expression has no admitted original programs".into());
        }
        tracing::debug!(target: "rumoca_sim::native_execution", programs=source.row_count(), selected=admitted, whole=full.is_some(), "portable ME scalar admission");
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
    ) -> Result<Option<f64>, String> {
        let Some(compiled) = self.selected.get(program).and_then(Option::as_ref) else {
            return Ok(None);
        };
        if !tables.is_empty() {
            return Ok(None);
        }
        profile::validate_inputs(&self.layout, y.len(), p.len(), tables.len())?;
        let mut value = [0.0];
        compiled
            .call(y, p, t, &mut value)
            .map_err(|error| error.to_string())?;
        Ok(Some(value[0]))
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
    ) -> Result<bool, String> {
        let Some(value) = self.selected_value(program, y, p, t, tables)? else {
            return Ok(false);
        };
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
    ) -> Result<Option<f64>, String> {
        if coordinate.1 != 0 {
            return Ok(None);
        }
        self.selected_value(coordinate.0, y, p, t, tables)
    }

    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), String> {
        let compiled = self
            .full
            .as_ref()
            .ok_or("WASM ME whole expression is unsupported")?;
        profile::validate_inputs(&self.layout, y.len(), p.len(), tables.len())?;
        if out.len() != self.source.output_count() {
            return Err("WASM ME output length differs from the original block".into());
        }
        let mut scratch = self.scratch.borrow_mut();
        compiled
            .call(y, p, t, &mut scratch)
            .map_err(|error| error.to_string())?;
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
        WasmExpression::compile(block, &self.layout, false)
            .map(|compiled| Rc::new(compiled) as Rc<_>)
    }
    fn compile_selectable_expression(
        &self,
        block: &ScalarProgramBlock,
    ) -> Result<Rc<dyn rumoca_solver::CompiledSolveExpression>, String> {
        WasmExpression::compile(block, &self.layout, true)
            .map(|compiled| Rc::new(compiled) as Rc<_>)
    }
    fn compile_jacobian_expression(
        &self,
        _: &ScalarProgramBlock,
    ) -> Result<Rc<dyn rumoca_solver::CompiledSolveJacobianExpression>, String> {
        Err("WASM ME directional execution is not admitted".into())
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

pub(crate) fn admitted_native_execution_backend(
    opts: &rumoca_solver::SimOptions,
    model: &rumoca_ir_solve::SolveModel,
) -> Option<rumoca_solver::fmi_me::MeExecutionBackend> {
    if !profile::model_context_admitted(
        opts.execution_policy,
        model.state_scalar_count(),
        model.external_tables.len(),
    ) {
        return None;
    }
    Some(rumoca_solver::fmi_me::MeExecutionBackend::new(Rc::new(
        WasmExecutionBackend {
            layout: model.problem.layout.clone(),
            calls: model.pure_calls.clone(),
        },
    )))
}
