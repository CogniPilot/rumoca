//! Value proof of a lazily activated numeric memory producer.
//!
//! The same checked conditional governs the monitored ratio and held step.
//! The inactive result is the producer's exact private P slot, never a
//! fabricated replacement step. This extends the existing shared term proof.

use super::*;
use std::sync::Arc;

pub(super) fn indicator(
    row: usize,
    step: UnaryOp,
    program: &[LinearOp],
    target: usize,
    held_load: Option<usize>,
) -> Result<Vec<LinearOp>, SolveProblemShapeContractError> {
    if let [
        prefix @ ..,
        LinearOp::Unary { dst, op, arg },
        LinearOp::StoreOutput { src },
    ] = program
        && matches!(step, UnaryOp::Floor | UnaryOp::Trunc)
        && *op == step
        && dst == src
    {
        return ratio_indicator(prefix, *arg).ok_or_else(|| failure(row));
    }
    let [
        prefix @ ..,
        LinearOp::FunctionConditional {
            dst_start,
            capture_start,
            program: conditional,
        },
        LinearOp::StoreOutput { src },
    ] = program
    else {
        return Err(failure(row));
    };
    if dst_start != src
        || conditional.owner.is_some()
        || conditional.result_count != 1
        || conditional.arms.len() != 1
        || !matches!(step, UnaryOp::Floor | UnaryOp::Trunc)
    {
        return Err(failure(row));
    }
    let arm = &conditional.arms[0];
    let [
        selected_prefix @ ..,
        LinearOp::Unary { dst, op, arg },
        LinearOp::StoreOutput { src },
    ] = arm.result.as_slice()
    else {
        return Err(failure(row));
    };
    if dst != src || *op != step {
        return Err(failure(row));
    }
    let first = Reg::try_from(arm.result_register_count).map_err(|_| failure(row))?;
    let selected = ratio_indicator_at(selected_prefix, *arg, first).ok_or_else(|| failure(row))?;
    let expected = conditional_program(conditional, selected, constant_output(1.0))
        .ok_or_else(|| failure(row))?;
    let actual_hold = conditional_program(
        conditional,
        constant_output(0.0),
        conditional.fallback.clone(),
    )
    .ok_or_else(|| failure(row))?;
    let expected_hold = conditional_program(
        conditional,
        constant_output(0.0),
        vec![
            LinearOp::LoadP {
                dst: 0,
                index: target,
            },
            LinearOp::StoreOutput { src: 0 },
        ],
    )
    .ok_or_else(|| failure(row))?;
    let actual_hold = projected(prefix, *dst_start, *capture_start, actual_hold);
    let expected_hold = projected(prefix, *dst_start, *capture_start, expected_hold);
    crate::linear_op::check_same_output(&actual_hold, 0, &expected_hold, 0).map_err(|_| {
        event_iteration_contract(
            row,
            "inactive quotient memory does not hold its exact private slot",
        )
    })?;
    if let Some(position) = held_load {
        verify_held_load(
            row,
            prefix,
            *dst_start,
            *capture_start,
            conditional,
            position,
        )?;
    }
    Ok(projected(prefix, *dst_start, *capture_start, expected))
}

fn conditional_program(
    original: &FunctionConditionalProgram,
    selected: Vec<LinearOp>,
    fallback: Vec<LinearOp>,
) -> Option<FunctionConditionalProgram> {
    FunctionConditionalProgram::checked(
        original.capture_count,
        vec![1],
        [(original.arms[0].condition.clone(), selected)],
        fallback,
    )
    .ok()
}

fn constant_output(value: f64) -> Vec<LinearOp> {
    vec![
        LinearOp::Const { dst: 0, value },
        LinearOp::StoreOutput { src: 0 },
    ]
}

fn projected(
    prefix: &[LinearOp],
    dst: Reg,
    captures: Reg,
    conditional: FunctionConditionalProgram,
) -> Vec<LinearOp> {
    let mut result = prefix.to_vec();
    result.push(LinearOp::FunctionConditional {
        dst_start: dst,
        capture_start: captures,
        program: Arc::new(conditional),
    });
    result.push(LinearOp::StoreOutput { src: dst });
    result
}

fn failure(row: usize) -> SolveProblemShapeContractError {
    event_iteration_contract(
        row,
        "quotient memory does not publish its declared activated rounding step",
    )
}

/// Substitute one exact reaching load in the existing term proof. The first
/// hold proof already excludes a literal fallback. This second proof binds
/// the held result to this load's register version through Moves/captures,
/// distinguishing another unshifted load of the very same base slot.
fn verify_held_load(
    row: usize,
    prefix: &[LinearOp],
    dst: Reg,
    captures: Reg,
    conditional: &FunctionConditionalProgram,
    position: usize,
) -> Result<(), SolveProblemShapeContractError> {
    let Some(LinearOp::LoadP { dst: loaded, .. }) = prefix.get(position) else {
        return Err(failure(row));
    };
    let mut marked = prefix.to_vec();
    marked[position] = LinearOp::Const {
        dst: *loaded,
        value: 0.0,
    };
    let actual = conditional_program(
        conditional,
        constant_output(0.0),
        conditional.fallback.clone(),
    )
    .ok_or_else(|| failure(row))?;
    let expected = conditional_program(conditional, constant_output(0.0), constant_output(0.0))
        .ok_or_else(|| failure(row))?;
    crate::linear_op::check_same_output(
        &projected(&marked, dst, captures, actual),
        0,
        &projected(&marked, dst, captures, expected),
        0,
    )
    .map_err(|_| {
        event_iteration_contract(
            row,
            "numeric memory fallback does not consume its exact affine held load",
        )
    })
}
