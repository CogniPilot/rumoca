//! Checked prepared value families return private tuples, never solver-Y writes.
use super::profile;
use rumoca_eval_solve::{PreparedTargetValuePlan, RowEvalContext};
use rumoca_exec_wasm::CompiledPrivateProgramWasm;
use rumoca_ir_solve::{SolvePureCallTable, VarLayout};
use std::cell::RefCell;

pub(super) struct WasmTargetValues {
    compiled: CompiledPrivateProgramWasm,
    layout: VarLayout,
    calls: SolvePureCallTable,
    scratch: RefCell<Vec<f64>>,
}

impl WasmTargetValues {
    pub(super) fn compile(
        plan: &PreparedTargetValuePlan,
        layout: &VarLayout,
        calls: &SolvePureCallTable,
        context: RowEvalContext<'_>,
    ) -> Result<Self, String> {
        validate_context(context)?;
        if context.pure_calls != Some(calls) {
            return Err("target-value call table differs from its model owner".into());
        }
        let compiled =
            rumoca_exec_wasm::compile_private_program_wasm(plan.program(), layout, calls)
                .map_err(|error| error.to_string())?;
        if plan.private_result_offset() >= compiled.output_count() {
            return Err("target-value projection exceeds private tuple".into());
        }
        let count = usize::try_from(compiled.scratch_bytes())
            .map_err(|_| "target-value scratch exceeds usize")?
            .div_ceil(8);
        let mut scratch = Vec::new();
        scratch
            .try_reserve_exact(count)
            .map_err(|error| error.to_string())?;
        scratch.resize(count, 0.0);
        Ok(Self {
            compiled,
            layout: layout.clone(),
            calls: calls.clone(),
            scratch: RefCell::new(scratch),
        })
    }
}

fn validate_context(context: RowEvalContext<'_>) -> Result<(), String> {
    if context.seed.is_some()
        || context
            .external_tables
            .is_some_and(|tables| !tables.is_empty())
    {
        return Err(
            "target-value context is outside the admitted primal table-free profile".into(),
        );
    }
    Ok(())
}

impl rumoca_solver::CompiledSolveTargetValues for WasmTargetValues {
    fn call(
        &self,
        selected: usize,
        y: &[f64],
        p: &[f64],
        time: f64,
        context: RowEvalContext<'_>,
    ) -> Result<f64, String> {
        validate_context(context)?;
        if context.pure_calls != Some(&self.calls) {
            return Err("target-value call table changed after admission".into());
        }
        profile::validate_inputs(
            &self.layout,
            y.len(),
            p.len(),
            context
                .external_tables
                .map_or(0, <[rumoca_core::ExternalTableData]>::len),
        )?;
        if selected >= self.compiled.output_count() {
            return Err("target-value selection exceeds issued private tuple".into());
        }
        let mut scratch = self.scratch.borrow_mut();
        self.compiled
            .call(y, p, time, &mut scratch)
            .map_err(|error| error.to_string())?;
        Ok(scratch[selected])
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn target_value_directional_context_is_not_zero_padded() {
        let seed = [];
        assert!(
            validate_context(RowEvalContext {
                seed: Some(&seed),
                ..Default::default()
            })
            .is_err()
        );
        assert!(validate_context(RowEvalContext::default()).is_ok());
    }
}
