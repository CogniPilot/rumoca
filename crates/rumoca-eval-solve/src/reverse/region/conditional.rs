use super::*;
use crate::{LazyTraceStep, PreparedLazyRowPlan};

// The evaluator's demand trace identifies the instructions needed by scalar
// selects inside an arm. Replay only those definitions in source order;
// register-version history then preserves the same selected values.
pub(in crate::reverse) fn demanded(
    inputs: &ReverseInputs<'_>,
    row: &[LinearOp],
    width: usize,
    captures: Option<&[f64]>,
) -> Result<Option<Vec<bool>>, EvalSolveError> {
    let Some(plan) =
        PreparedLazyRowPlan::with_conditional_captures(row, width, captures.map(<[f64]>::len))
    else {
        return Ok(None);
    };
    let mut scratch = RowEvalScratch::default();
    let mut values = vec![0.; plan.outputs.len()];
    let mut point = PreparedRowEval::new(row, width, inputs.y, inputs.p, inputs.t, inputs.context)
        .with_lazy_plan(Some(&plan));
    if let Some(captures) = captures {
        point = point.with_conditional_captures(captures);
    }
    crate::eval_row_prepared_lazy(point, &mut scratch, &mut OutputCursor::new(&mut values))?;
    let mut mask = if plan.versions.is_some() {
        scratch.version_computed
    } else {
        vec![false; row.len()]
    };
    for step in scratch.lazy_trace {
        match step {
            LazyTraceStep::Apply(i) => mask[i] = true,
            LazyTraceStep::Select { dst, .. } => mask[plan.definitions[dst as usize]] = true,
            _ => (),
        }
    }
    for (i, op) in row.iter().enumerate() {
        if matches!(op, LinearOp::StoreOutput { .. }) {
            mask[i] = true;
        }
    }
    Ok(Some(mask))
}

pub(super) fn selected(
    inputs: &ReverseInputs<'_>,
    program: &FunctionConditionalProgram,
    captures: &[f64],
) -> Result<(Frame, Vec<f64>), EvalSolveError> {
    let point = PreparedRowEval::new(&[], 0, inputs.y, inputs.p, inputs.t, inputs.context);
    let mut pool = Vec::new();
    let mut selected = (program.fallback.as_slice(), program.fallback_register_count);
    for arm in &program.arms {
        let mut predicate = [0.];
        crate::eval_function_conditional_region(
            point,
            &arm.condition,
            arm.condition_register_count,
            captures,
            &mut pool,
            &mut predicate,
        )?;
        if predicate[0] != 0. {
            selected = (&arm.result, arm.result_register_count);
            break;
        }
    }
    let (row, width) = selected;
    let mask = demanded(inputs, row, width, Some(captures))?;
    let mut frame = Frame::new(width, &[], captures)?;
    let values = frame.run(inputs, row, &[], mask.as_deref())?;
    Ok((frame, values))
}
