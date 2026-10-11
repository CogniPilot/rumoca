//! Aggregate copy execution using the shared read-before-write admission.

use super::*;

pub(super) fn eval(
    regs: &mut [f64],
    results: &mut Vec<f64>,
    op: &LinearOp,
    snapshot: tensor_snapshot::Snapshot,
) -> Result<(), EvalSolveError> {
    let staged = snapshot.width() != 0;
    if staged {
        reserve_results(results, snapshot.width())?;
        results.resize(snapshot.width(), 0.0);
    }
    let dst = op.dst_register().expect("tensor copy has a destination") as usize;
    match op {
        LinearOp::TensorConcatenate {
            sources,
            dimensions,
            axis,
            lanes,
            ..
        } => {
            visit_tensor_concatenate(sources, dimensions, *axis, *lanes, |source, offset| {
                commit_copy(regs, results, staged, dst, source, offset);
                Ok::<_, EvalSolveError>(())
            })?;
        }
        LinearOp::TensorUpdate {
            base_start,
            value_start,
            dimensions,
            subscripts,
            lanes,
            ..
        } => {
            for element in 0..snapshot.result_width() / lanes {
                let value_offset = tensor_update_register_value_offset(
                    dimensions,
                    subscripts,
                    element,
                    |register| Ok::<_, EvalSolveError>(regs[register as usize]),
                )?;
                for lane in 0..*lanes {
                    let source = match value_offset {
                        Some(offset) => *value_start + (offset * lanes + lane) as Reg,
                        None => *base_start + (element * lanes + lane) as Reg,
                    };
                    commit_copy(regs, results, staged, dst, source, element * lanes + lane);
                }
            }
        }
        LinearOp::TensorFill {
            value_start,
            count,
            lanes,
            ..
        } => {
            for element in 0..*count {
                for lane in 0..*lanes {
                    commit_copy(
                        regs,
                        results,
                        staged,
                        dst,
                        *value_start + lane as Reg,
                        element * lanes + lane,
                    );
                }
            }
        }
        _ => unreachable!("native copy dispatch accepts only concatenate, update and fill"),
    }
    if staged {
        regs[dst..dst + snapshot.width()].copy_from_slice(results);
    }
    Ok(())
}

fn commit_copy(
    regs: &mut [f64],
    results: &mut [f64],
    staged: bool,
    dst: usize,
    source: Reg,
    offset: usize,
) {
    let value = regs[source as usize];
    if staged {
        results[offset] = value;
    } else {
        regs[dst + offset] = value;
    }
}
