use rumoca_ir_solve::{ScalarProgramBlock, ScalarSlot};

use crate::{EvalSolveError, RowEvalContext, eval_scalar_program_block_with_context};

pub struct UpdateRowApplication<'a> {
    pub block: &'a ScalarProgramBlock,
    pub targets: &'a [ScalarSlot],
    pub y: &'a mut [f64],
    pub p: &'a mut [f64],
    pub t: f64,
    pub context: RowEvalContext<'a>,
    pub tol: f64,
    pub max_iters: usize,
}

pub fn eval_and_apply_update_rows(
    application: UpdateRowApplication<'_>,
) -> Result<bool, EvalSolveError> {
    if application.block.is_empty() {
        return Ok(false);
    }
    validate_update_shape(application.block, application.targets)?;

    let mut values = update_row_value_buffer(application.block.len())?;
    let mut changed_any = false;
    for _ in 0..application.max_iters {
        let eval_y = update_row_snapshot(application.y, "update row y snapshot")?;
        let eval_p = update_row_snapshot(application.p, "update row p snapshot")?;
        eval_scalar_program_block_with_context(
            application.block,
            eval_y.as_slice(),
            eval_p.as_slice(),
            application.t,
            application.context,
            &mut values,
        )?;
        let changed = apply_scalar_slot_values(
            application.targets,
            values.as_slice(),
            application.y,
            application.p,
            application.tol,
        )?;
        if !changed {
            return Ok(changed_any);
        }
        changed_any = true;
    }
    Err(EvalSolveError::UpdateDidNotConverge {
        t: application.t,
        max_iters: application.max_iters,
    })
}

pub fn apply_scalar_slot_values(
    targets: &[ScalarSlot],
    values: &[f64],
    y: &mut [f64],
    p: &mut [f64],
    tol: f64,
) -> Result<bool, EvalSolveError> {
    if targets.len() != values.len() {
        return Err(EvalSolveError::UpdateRowTargetMismatch {
            rows: values.len(),
            targets: targets.len(),
        });
    }
    let mut changed = false;
    for (target, value) in targets.iter().zip(values.iter().copied()) {
        changed |= apply_scalar_slot_value(*target, value, y, p, tol)?;
    }
    Ok(changed)
}

pub fn apply_scalar_slot_value(
    target: ScalarSlot,
    value: f64,
    y: &mut [f64],
    p: &mut [f64],
    tol: f64,
) -> Result<bool, EvalSolveError> {
    match target {
        ScalarSlot::Y { index, .. } => update_indexed_slot("y", y, index, value, tol),
        ScalarSlot::P { index, .. } => update_indexed_slot("p", p, index, value, tol),
        ScalarSlot::Time | ScalarSlot::Constant(_) => Ok(false),
    }
}

fn validate_update_shape(
    block: &ScalarProgramBlock,
    targets: &[ScalarSlot],
) -> Result<(), EvalSolveError> {
    if block.len() == targets.len() {
        return Ok(());
    }
    Err(EvalSolveError::UpdateRowTargetMismatch {
        rows: block.len(),
        targets: targets.len(),
    })
}

fn update_indexed_slot(
    vector: &'static str,
    slots: &mut [f64],
    index: usize,
    value: f64,
    tol: f64,
) -> Result<bool, EvalSolveError> {
    let len = slots.len();
    let slot = slots.get_mut(index).ok_or(EvalSolveError::MissingInput {
        vector,
        index,
        len,
        span: None,
    })?;
    let changed = (*slot - value).abs() > tol;
    if changed {
        *slot = value;
    }
    Ok(changed)
}

fn update_row_value_buffer(len: usize) -> Result<Vec<f64>, EvalSolveError> {
    let mut values = update_row_vec_with_capacity(len, "update row value count")?;
    values.resize(len, 0.0);
    Ok(values)
}

fn update_row_snapshot(values: &[f64], context: &'static str) -> Result<Vec<f64>, EvalSolveError> {
    let mut snapshot = update_row_vec_with_capacity(values.len(), context)?;
    snapshot.extend_from_slice(values);
    Ok(snapshot)
}

fn update_row_vec_with_capacity<T>(
    capacity: usize,
    context: &'static str,
) -> Result<Vec<T>, EvalSolveError> {
    let mut values = Vec::new();
    values
        .try_reserve_exact(capacity)
        .map_err(|_| EvalSolveError::InvalidRow {
            message: format!("{context} exceeds host memory limits"),
            span: None,
        })?;
    Ok(values)
}

#[cfg(test)]
mod tests {
    use rumoca_ir_solve::{LinearOp, ScalarProgramBlock, scalar_slot_p};

    use super::*;

    fn fixture_span() -> rumoca_core::Span {
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("eval_solve_update_rows_source_46.mo"),
            0,
            1,
        )
    }

    #[test]
    fn update_rows_apply_scalar_slot_targets_until_stable() {
        let block = ScalarProgramBlock::with_source_span(
            vec![vec![
                LinearOp::Const { dst: 0, value: 2.0 },
                LinearOp::StoreOutput { src: 0 },
            ]],
            fixture_span(),
        );
        let mut y = vec![1.0];
        let mut p = vec![0.0];
        let changed = eval_and_apply_update_rows(UpdateRowApplication {
            block: &block,
            targets: &[scalar_slot_p(0)],
            y: &mut y,
            p: &mut p,
            t: 0.0,
            context: RowEvalContext::default(),
            tol: 1.0e-12,
            max_iters: 4,
        })
        .expect("update rows should evaluate");

        assert!(changed);
        assert_eq!(p, vec![2.0]);
    }

    #[test]
    fn update_row_vec_with_capacity_rejects_impossible_capacity() {
        let err = update_row_vec_with_capacity::<u8>(usize::MAX, "update row test vector")
            .expect_err("impossible update row capacity should fail");

        assert!(matches!(err, EvalSolveError::InvalidRow { .. }));
        assert!(
            err.to_string()
                .contains("update row test vector exceeds host memory limits")
        );
    }
}
