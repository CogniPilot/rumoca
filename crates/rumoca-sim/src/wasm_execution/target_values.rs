//! Checked prepared value families return private tuples, never solver-Y writes.
use super::profile;
use rumoca_eval_solve::{PreparedTargetValuePlan, RowEvalContext};
use rumoca_ir_solve::{SolvePureCallTable, VarLayout};
use std::{cell::RefCell, rc::Rc};

pub(super) struct WasmTargetValues {
    compiled: Rc<super::batch::Entry>,
    // The batch's layout, shared: thousands of families are issued on large
    // models, and a copy per family held gigabytes.
    layout: Rc<VarLayout>,
    // Shared, never copied: only its owner lineage is checked per call, and a
    // deep copy per value family dominated WASM session start-up.
    calls: Rc<SolvePureCallTable>,
    scratch: RefCell<Vec<f64>>,
}

impl WasmTargetValues {
    pub(super) fn compile(
        plan: &PreparedTargetValuePlan,
        calls: &Rc<SolvePureCallTable>,
        batch: &super::batch::ModuleBatch,
        context: RowEvalContext<'_>,
    ) -> Result<Self, String> {
        validate_context(context)?;
        validate_call_owner(context, calls)?;
        let compiled = batch.request(plan.program().clone(), None)?;
        if plan.private_result_offset() >= compiled.output_count() {
            return Err("target-value projection exceeds private tuple".into());
        }
        // Sized on the first call, once the shared module is emitted.
        Ok(Self {
            compiled,
            layout: Rc::clone(batch.layout()),
            calls: Rc::clone(calls),
            scratch: RefCell::new(Vec::new()),
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

fn validate_call_owner(
    context: RowEvalContext<'_>,
    calls: &SolvePureCallTable,
) -> Result<(), String> {
    if !context
        .pure_calls
        .is_some_and(|source| source.shares_table_owner(calls))
    {
        return Err("target-value call table differs from its model owner".into());
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
    ) -> Result<f64, rumoca_solver::RuntimeSolveError> {
        validate_context(context)?;
        validate_call_owner(context, &self.calls)?;
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
        let compiled = self.compiled.code()?;
        let mut scratch = self.scratch.borrow_mut();
        let cells = usize::try_from(compiled.scratch_bytes())
            .map_err(|_| "target-value scratch exceeds usize")?
            .div_ceil(8);
        if let Some(missing) = cells.checked_sub(scratch.len()).filter(|&n| n > 0) {
            scratch
                .try_reserve_exact(missing)
                .map_err(|error| error.to_string())?;
            scratch.resize(cells, 0.0);
        }
        compiled
            .call(y, p, time, &mut scratch)
            .map_err(rumoca_solver::runtime::native_wasm_error::execution_error)?;
        Ok(scratch[selected])
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn target_value_context_binds_the_exact_call_table_owner() {
        let calls = SolvePureCallTable::default();
        let cloned = calls.clone();
        let foreign = SolvePureCallTable::default();
        assert_eq!(calls, foreign);
        assert!(
            validate_call_owner(
                RowEvalContext {
                    pure_calls: Some(&cloned),
                    ..Default::default()
                },
                &calls
            )
            .is_ok()
        );
        assert!(
            validate_call_owner(
                RowEvalContext {
                    pure_calls: Some(&foreign),
                    ..Default::default()
                },
                &calls
            )
            .is_err()
        );
        assert!(validate_call_owner(RowEvalContext::default(), &calls).is_err());
    }

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
