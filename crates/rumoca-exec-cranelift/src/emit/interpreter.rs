//! Interpreter fallback for evaluating row plans without JIT.
//!
//! These functions mirror the JIT-compiled row evaluators but run as a pure
//! Rust interpreter, used for validation (cross-checking JIT output) and as
//! a fallback when the JIT path is not available.

use super::input_validation::{row_input_requirements, validate_input_requirements};
use super::{
    CompileError, GeneralRowPlan, RowInputs, RowPlan, SimpleOp, SimpleRowPlan, input_compile_error,
};
use rumoca_core::ExternalTableData;
use rumoca_eval_solve::{
    try_eval_table_bound_value_in, try_eval_table_lookup_slope_value_in,
    try_eval_table_lookup_value_in, try_eval_time_table_next_event_value_in,
};
use rumoca_ir_solve::{BinaryOp, CompareOp, LinearOp, UnaryOp, resolve_indexed_slot};

#[inline(always)]
pub(super) fn execute_row(
    row: &RowPlan,
    regs_scratch: &mut Vec<f64>,
    inputs: RowInputs<'_>,
    out: &mut [f64],
) -> Result<(), CompileError> {
    validate_input_requirements(row_input_requirements(row), inputs.y, inputs.p, inputs.seed)?;
    match row {
        RowPlan::Simple(row) => {
            execute_simple_row(row, regs_scratch, inputs.y, inputs.p, inputs.t, out)
        }
        RowPlan::General(row) => execute_general_row(row, regs_scratch, inputs, out),
    }
}

#[inline(always)]
fn execute_simple_row(
    row: &SimpleRowPlan,
    regs_scratch: &mut Vec<f64>,
    y: &[f64],
    p: &[f64],
    t: f64,
    out: &mut [f64],
) -> Result<(), CompileError> {
    let regs = runtime_reg_slice(regs_scratch, row.reg_count);
    for op in row.ops.iter().copied() {
        match op {
            SimpleOp::Const { dst, value } => set_reg_value(regs, dst as usize, value),
            SimpleOp::LoadTime { dst } => set_reg_value(regs, dst as usize, t),
            SimpleOp::LoadY { dst, index } => {
                let value = read_input_value("y", y, index as usize)?;
                set_reg_value(regs, dst as usize, value)
            }
            SimpleOp::LoadP { dst, index } => {
                let value = read_input_value("p", p, index as usize)?;
                set_reg_value(regs, dst as usize, value)
            }
            SimpleOp::Unary { dst, op, arg } => {
                let x = read_reg_value(regs, arg as usize);
                set_reg_value(regs, dst as usize, apply_unary(op, x));
            }
            SimpleOp::Binary { dst, op, lhs, rhs } => {
                let lhs = read_reg_value(regs, lhs as usize);
                let rhs = read_reg_value(regs, rhs as usize);
                set_reg_value(regs, dst as usize, apply_binary(op, lhs, rhs));
            }
            SimpleOp::Compare { dst, op, lhs, rhs } => {
                let lhs = read_reg_value(regs, lhs as usize);
                let rhs = read_reg_value(regs, rhs as usize);
                set_reg_value(regs, dst as usize, apply_compare(op, lhs, rhs));
            }
            SimpleOp::Select {
                dst,
                cond,
                if_true,
                if_false,
            } => {
                let cond = read_reg_value(regs, cond as usize);
                let if_true = read_reg_value(regs, if_true as usize);
                let if_false = read_reg_value(regs, if_false as usize);
                set_reg_value(
                    regs,
                    dst as usize,
                    if cond != 0.0 { if_true } else { if_false },
                );
            }
        }
    }
    for (slot, &src) in out.iter_mut().zip(row.output_srcs.iter()) {
        *slot = read_reg_value(regs, src);
    }
    Ok(())
}

#[inline(always)]
fn execute_general_row(
    row: &GeneralRowPlan,
    regs_scratch: &mut Vec<f64>,
    inputs: RowInputs<'_>,
    out: &mut [f64],
) -> Result<(), CompileError> {
    let regs = runtime_reg_slice(regs_scratch, row.reg_count);
    for op in row.ops.iter().copied() {
        execute_general_op(
            regs,
            op,
            inputs.y,
            inputs.p,
            inputs.t,
            inputs.seed,
            inputs.external_tables,
        )?;
    }
    for (slot, &src) in out.iter_mut().zip(row.output_srcs.iter()) {
        *slot = read_reg_value(regs, src);
    }
    Ok(())
}

#[inline(always)]
fn execute_general_op(
    regs: &mut [f64],
    op: LinearOp,
    y: &[f64],
    p: &[f64],
    t: f64,
    seed: Option<&[f64]>,
    external_tables: &[ExternalTableData],
) -> Result<(), CompileError> {
    match op {
        LinearOp::Const { dst, value } => set_reg_value(regs, dst as usize, value),
        LinearOp::LoadTime { dst } => set_reg_value(regs, dst as usize, t),
        LinearOp::LoadY { dst, index } => {
            let value = read_input_value("y", y, index)?;
            set_reg_value(regs, dst as usize, value);
        }
        LinearOp::LoadP { dst, index } => {
            let value = read_input_value("p", p, index)?;
            set_reg_value(regs, dst as usize, value);
        }
        LinearOp::LoadIndexedP {
            dst,
            base,
            count,
            index,
        } => {
            let slot = resolve_indexed_slot(read_reg_value(regs, index as usize), base, count);
            let value = read_input_value("p", p, slot)?;
            set_reg_value(regs, dst as usize, value);
        }
        LinearOp::LoadSeed { dst, index } => {
            let seed = seed.ok_or_else(|| input_compile_error("seed", index, 0))?;
            let value = read_input_value("seed", seed, index)?;
            set_reg_value(regs, dst as usize, value);
        }
        LinearOp::LoadIndexedSeed {
            dst,
            base,
            count,
            index,
        } => {
            let seed = seed.ok_or_else(|| input_compile_error("seed", base, 0))?;
            let slot = resolve_indexed_slot(read_reg_value(regs, index as usize), base, count);
            let value = read_input_value("seed", seed, slot)?;
            set_reg_value(regs, dst as usize, value);
        }
        LinearOp::Move { dst, src } => {
            let value = read_reg_value(regs, src as usize);
            set_reg_value(regs, dst as usize, value);
        }
        LinearOp::LinearSolveComponent { dst, .. } => {
            set_reg_value(regs, dst as usize, eval_linear_solve_component(regs, op));
        }
        LinearOp::TableBounds { dst, table_id, max } => {
            let table_id = read_reg_value(regs, table_id as usize);
            let value =
                try_eval_table_bound_value_in(table_id, max, external_tables).ok_or_else(|| {
                    table_compile_error(
                        if max { "bounds max" } else { "bounds min" },
                        table_id,
                        None,
                    )
                })?;
            set_reg_value(regs, dst as usize, value);
        }
        LinearOp::TableLookup { .. }
        | LinearOp::TableLookupSlope { .. }
        | LinearOp::TableNextEvent { .. } => execute_general_table_op(regs, op, external_tables)?,
        LinearOp::RandomInitialState { dst, .. }
        | LinearOp::RandomResult { dst, .. }
        | LinearOp::RandomState { dst, .. }
        | LinearOp::ImpureRandomInit { dst, .. }
        | LinearOp::ImpureRandom { dst, .. }
        | LinearOp::ImpureRandomInteger { dst, .. } => set_reg_value(regs, dst as usize, 1.0),
        LinearOp::Unary { dst, op, arg } => {
            let x = read_reg_value(regs, arg as usize);
            set_reg_value(regs, dst as usize, apply_unary(op, x));
        }
        LinearOp::Binary { dst, op, lhs, rhs } => {
            let lhs = read_reg_value(regs, lhs as usize);
            let rhs = read_reg_value(regs, rhs as usize);
            set_reg_value(regs, dst as usize, apply_binary(op, lhs, rhs));
        }
        LinearOp::Compare { dst, op, lhs, rhs } => {
            let lhs = read_reg_value(regs, lhs as usize);
            let rhs = read_reg_value(regs, rhs as usize);
            set_reg_value(regs, dst as usize, apply_compare(op, lhs, rhs));
        }
        LinearOp::Select {
            dst,
            cond,
            if_true,
            if_false,
        } => {
            let cond = read_reg_value(regs, cond as usize);
            let if_true = read_reg_value(regs, if_true as usize);
            let if_false = read_reg_value(regs, if_false as usize);
            set_reg_value(
                regs,
                dst as usize,
                if cond != 0.0 { if_true } else { if_false },
            );
        }
        LinearOp::StoreOutput { .. } => {}
    }
    Ok(())
}

fn execute_general_table_op(
    regs: &mut [f64],
    op: LinearOp,
    external_tables: &[ExternalTableData],
) -> Result<(), CompileError> {
    match op {
        LinearOp::TableLookup {
            dst,
            table_id,
            column,
            input,
        } => {
            let table_id = read_reg_value(regs, table_id as usize);
            let column = read_reg_value(regs, column as usize);
            let input = read_reg_value(regs, input as usize);
            let value = try_eval_table_lookup_value_in(table_id, column, input, external_tables)
                .ok_or_else(|| table_compile_error("lookup", table_id, Some(column)))?;
            set_reg_value(regs, dst as usize, value);
        }
        LinearOp::TableLookupSlope {
            dst,
            table_id,
            column,
            input,
        } => {
            let table_id = read_reg_value(regs, table_id as usize);
            let column = read_reg_value(regs, column as usize);
            let input = read_reg_value(regs, input as usize);
            let value =
                try_eval_table_lookup_slope_value_in(table_id, column, input, external_tables)
                    .ok_or_else(|| table_compile_error("lookup slope", table_id, Some(column)))?;
            set_reg_value(regs, dst as usize, value);
        }
        LinearOp::TableNextEvent {
            dst,
            table_id,
            time,
        } => {
            let table_id = read_reg_value(regs, table_id as usize);
            let time = read_reg_value(regs, time as usize);
            let value = try_eval_time_table_next_event_value_in(table_id, time, external_tables)
                .ok_or_else(|| table_compile_error("next event", table_id, None))?;
            set_reg_value(regs, dst as usize, value);
        }
        _ => {}
    }
    Ok(())
}

fn table_compile_error(
    operation: &'static str,
    table_id: f64,
    column: Option<f64>,
) -> CompileError {
    let message = if let Some(column) = column {
        format!("external table {operation} failed for table id {table_id} column {column}")
    } else {
        format!("external table {operation} failed for table id {table_id}")
    };
    CompileError::Input(message)
}

fn read_input_value(
    vector: &'static str,
    values: &[f64],
    index: usize,
) -> Result<f64, CompileError> {
    values
        .get(index)
        .copied()
        .ok_or_else(|| input_compile_error(vector, index, values.len()))
}

#[inline(always)]
fn runtime_reg_slice(regs_scratch: &mut Vec<f64>, reg_count: usize) -> &mut [f64] {
    if regs_scratch.len() < reg_count {
        regs_scratch.resize(reg_count, 0.0);
    }
    &mut regs_scratch[..reg_count]
}

#[inline(always)]
fn set_reg_value(regs: &mut [f64], reg: usize, value: f64) {
    regs[reg] = value;
}

#[inline(always)]
fn read_reg_value(regs: &[f64], reg: usize) -> f64 {
    regs[reg]
}

fn eval_linear_solve_component(regs: &[f64], op: LinearOp) -> f64 {
    let LinearOp::LinearSolveComponent {
        matrix_start,
        rhs_start,
        n,
        component,
        ..
    } = op
    else {
        return f64::NAN;
    };
    if n == 0 || component >= n {
        return f64::NAN;
    }
    let mut matrix = vec![0.0; n * n];
    let mut rhs = vec![0.0; n];
    for row in 0..n {
        rhs[row] = read_reg_value(regs, rhs_start as usize + row);
        for col in 0..n {
            matrix[row * n + col] = read_reg_value(regs, matrix_start as usize + row * n + col);
        }
    }
    solve_dense_component(&mut matrix, &mut rhs, n, component)
}

fn solve_dense_component(matrix: &mut [f64], rhs: &mut [f64], n: usize, component: usize) -> f64 {
    for col in 0..n {
        let Some(pivot) = pivot_row(matrix, n, col) else {
            return f64::NAN;
        };
        swap_dense_rows(matrix, rhs, n, col, pivot);
        let pivot_value = matrix[col * n + col];
        if pivot_value.abs() <= f64::EPSILON {
            return f64::NAN;
        }
        for row in col + 1..n {
            let factor = matrix[row * n + col] / pivot_value;
            matrix[row * n + col] = 0.0;
            for entry in col + 1..n {
                matrix[row * n + entry] -= factor * matrix[col * n + entry];
            }
            rhs[row] -= factor * rhs[col];
        }
    }

    let mut solution = vec![0.0; n];
    for row in (0..n).rev() {
        let tail = ((row + 1)..n)
            .map(|col| matrix[row * n + col] * solution[col])
            .sum::<f64>();
        solution[row] = (rhs[row] - tail) / matrix[row * n + row];
    }
    solution[component]
}

fn pivot_row(matrix: &[f64], n: usize, col: usize) -> Option<usize> {
    (col..n).max_by(|&lhs, &rhs| {
        matrix[lhs * n + col]
            .abs()
            .total_cmp(&matrix[rhs * n + col].abs())
    })
}

fn swap_dense_rows(matrix: &mut [f64], rhs: &mut [f64], n: usize, lhs: usize, rhs_row: usize) {
    if lhs == rhs_row {
        return;
    }
    for col in 0..n {
        matrix.swap(lhs * n + col, rhs_row * n + col);
    }
    rhs.swap(lhs, rhs_row);
}

#[inline(always)]
fn apply_unary(op: UnaryOp, value: f64) -> f64 {
    match op {
        UnaryOp::Neg => -value,
        UnaryOp::Not => {
            if value == 0.0 {
                1.0
            } else {
                0.0
            }
        }
        UnaryOp::Abs => value.abs(),
        UnaryOp::Sign => {
            if value > 0.0 {
                1.0
            } else if value < 0.0 {
                -1.0
            } else {
                0.0
            }
        }
        UnaryOp::Sqrt => value.sqrt(),
        UnaryOp::Floor => value.floor(),
        UnaryOp::Ceil => value.ceil(),
        UnaryOp::Trunc => value.trunc(),
        UnaryOp::Sin => value.sin(),
        UnaryOp::Cos => value.cos(),
        UnaryOp::Tan => value.tan(),
        UnaryOp::Asin => value.asin(),
        UnaryOp::Acos => value.acos(),
        UnaryOp::Atan => value.atan(),
        UnaryOp::Sinh => value.sinh(),
        UnaryOp::Cosh => value.cosh(),
        UnaryOp::Tanh => value.tanh(),
        UnaryOp::Exp => value.exp(),
        UnaryOp::Log => value.ln(),
        UnaryOp::Log10 => value.log10(),
    }
}

#[inline(always)]
fn apply_binary(op: BinaryOp, lhs: f64, rhs: f64) -> f64 {
    match op {
        BinaryOp::Add => lhs + rhs,
        BinaryOp::Sub => lhs - rhs,
        BinaryOp::Mul => lhs * rhs,
        BinaryOp::Div => {
            if rhs == 0.0 {
                if lhs == 0.0 { 0.0 } else { f64::INFINITY }
            } else {
                lhs / rhs
            }
        }
        BinaryOp::Pow => lhs.powf(rhs),
        BinaryOp::And => {
            if lhs != 0.0 && rhs != 0.0 {
                1.0
            } else {
                0.0
            }
        }
        BinaryOp::Or => {
            if lhs != 0.0 || rhs != 0.0 {
                1.0
            } else {
                0.0
            }
        }
        BinaryOp::Atan2 => lhs.atan2(rhs),
        BinaryOp::Min => lhs.min(rhs),
        BinaryOp::Max => lhs.max(rhs),
    }
}

#[inline(always)]
fn apply_compare(op: CompareOp, lhs: f64, rhs: f64) -> f64 {
    op.compare_as_f64(lhs, rhs)
}
