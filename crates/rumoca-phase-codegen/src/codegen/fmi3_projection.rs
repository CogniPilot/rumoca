use std::collections::{BTreeMap, BTreeSet};

use minijinja::Value;

use crate::errors::render_err;

use super::render_expr::get_field;
use super::render_solve::SolveRowValue;

pub fn fmi3_native_projection_available(
    problem: &rumoca_ir_solve::SolveProblem,
) -> Result<bool, crate::CodegenError> {
    let blocks = &problem.continuous.algebraic_projection_plan.blocks;
    if blocks.is_empty() {
        return Ok(false);
    }
    let scalar = rumoca_eval_solve::to_scalar_program_block(&problem.continuous.implicit_rhs)
        .map_err(|error| crate::CodegenError::template(error.to_string()))?;
    let mut program_for_output = BTreeMap::new();
    let mut cursor = 0usize;
    for (program_index, program) in scalar.programs.iter().enumerate() {
        let output_count = rumoca_ir_solve::ScalarProgramBlock::program_output_count(program);
        let end = cursor
            .checked_add(output_count)
            .ok_or_else(|| crate::CodegenError::template("FMI3 implicit output cursor overflow"))?;
        let Some(indices) = scalar.output_indices.get(cursor..end) else {
            return Ok(false);
        };
        if output_count != 1
            && indices
                .iter()
                .any(|index| *index >= problem.solve_layout.state_scalar_count)
        {
            return Ok(false);
        }
        if let [output] = indices
            && program_for_output.insert(*output, program_index).is_some()
        {
            return Ok(false);
        }
        cursor = end;
    }
    if cursor != scalar.output_indices.len() {
        return Ok(false);
    }
    let projection_start = problem.solve_layout.state_scalar_count;
    let projection_count = problem
        .solve_layout
        .algebraic_scalar_count
        .checked_add(problem.solve_layout.output_scalar_count)
        .ok_or_else(|| crate::CodegenError::template("FMI3 projection count overflow"))?;
    let projection_end = projection_start
        .checked_add(projection_count)
        .ok_or_else(|| crate::CodegenError::template("FMI3 projection range overflow"))?;
    let mut projected_y = BTreeSet::new();
    for block in blocks {
        let ([row_index], [y_index], []) = (
            block.rows.as_slice(),
            block.y_indices.as_slice(),
            block.causal_steps.as_slice(),
        ) else {
            return Ok(false);
        };
        if !(projection_start..projection_end).contains(y_index) || !projected_y.insert(*y_index) {
            return Ok(false);
        }
        let Some(program_index) = program_for_output.get(row_index).copied() else {
            return Ok(false);
        };
        let program = &scalar.programs[program_index];
        let supported = match rumoca_eval_solve::target_assignment_shape(program)
            .map_err(|error| crate::CodegenError::template(error.to_string()))?
        {
            Some(shape) => shape.target_y_index() == *y_index,
            None => !program.iter().any(
                |op| matches!(op, rumoca_ir_solve::LinearOp::LoadY { index, .. } if index == y_index),
            ),
        };
        if !supported {
            return Ok(false);
        }
    }
    Ok(projected_y.len() == projection_count)
}

fn usize_values(value: &Value, context: &str) -> Result<Vec<usize>, minijinja::Error> {
    value
        .try_iter()
        .map_err(|_| render_err(format!("{context} must be a sequence")))?
        .map(|item| {
            item.as_usize().ok_or_else(|| {
                render_err(format!("{context} entries must be non-negative integers"))
            })
        })
        .collect()
}

fn required_sequence_field(value: &Value, field: &str) -> Result<Vec<Value>, minijinja::Error> {
    Ok(get_field(value, field)?
        .try_iter()
        .map_err(|_| {
            render_err(format!(
                "FMI3 projection block `{field}` must be a sequence"
            ))
        })?
        .collect())
}

fn empty_schedule() -> Value {
    Value::from_serialize(Vec::<serde_json::Value>::new())
}

fn supports_assignment(row: &SolveRowValue, target: usize) -> Result<bool, minijinja::Error> {
    let ops = row.ops();
    match rumoca_eval_solve::target_assignment_shape(ops)
        .map_err(|error| render_err(format!("invalid FMI3 target-assignment row: {error}")))?
    {
        Some(shape) => Ok(shape.target_y_index() == target),
        None if !ops.iter().any(
            |op| matches!(op, rumoca_ir_solve::LinearOp::LoadY { index, .. } if *index == target),
        ) =>
        {
            Ok(true)
        }
        None => Ok(false),
    }
}

fn scalar_projection_schedule_miss<T>() -> Result<Option<T>, minijinja::Error> {
    Ok(Option::None)
}

fn scalar_projection_rows(
    blocks: &[Value],
    state_count: usize,
    projection_count: usize,
) -> Result<Option<Vec<(usize, usize)>>, minijinja::Error> {
    let projection_end = state_count
        .checked_add(projection_count)
        .ok_or_else(|| render_err("FMI3 projection range overflow"))?;
    let mut rows = Vec::new();
    rows.try_reserve_exact(blocks.len())
        .map_err(|_| render_err("FMI3 projection schedule exceeds host memory limits"))?;
    let mut projected_y = BTreeSet::new();
    for block in blocks {
        let block_rows = required_sequence_field(block, "rows")?;
        let y_indices = required_sequence_field(block, "y_indices")?;
        let causal_steps = required_sequence_field(block, "causal_steps")?;
        if block_rows.len() != 1 || y_indices.len() != 1 || !causal_steps.is_empty() {
            return scalar_projection_schedule_miss();
        }
        let row_index = block_rows[0]
            .as_usize()
            .ok_or_else(|| render_err("FMI3 projection row must be a non-negative integer"))?;
        let y_index = y_indices[0]
            .as_usize()
            .ok_or_else(|| render_err("FMI3 projection target must be a non-negative integer"))?;
        if !(state_count..projection_end).contains(&y_index) || !projected_y.insert(y_index) {
            return scalar_projection_schedule_miss();
        }
        rows.push((row_index, y_index));
    }
    Ok((projected_y.len() == projection_count).then_some(rows))
}

pub(super) fn fmi3_scalar_projection_schedule_function(
    blocks: Value,
    programs: Value,
    output_indices: Value,
    state_count: Value,
    projection_count: Value,
) -> Result<Value, minijinja::Error> {
    let state_count = state_count
        .as_usize()
        .ok_or_else(|| render_err("FMI3 Solve-IR state count must be a non-negative integer"))?;
    let projection_count = projection_count.as_usize().ok_or_else(|| {
        render_err("FMI3 Solve-IR projection count must be a non-negative integer")
    })?;
    let blocks = blocks
        .try_iter()
        .map_err(|_| render_err("FMI3 algebraic projection blocks must be a sequence"))?
        .collect::<Vec<_>>();
    if blocks.is_empty() {
        return Ok(empty_schedule());
    }

    let Some(projection_rows) = scalar_projection_rows(&blocks, state_count, projection_count)?
    else {
        return Ok(empty_schedule());
    };

    let programs = programs
        .try_iter()
        .map_err(|_| render_err("FMI3 implicit scalar programs must be a sequence"))?
        .collect::<Vec<_>>();
    let output_indices = usize_values(&output_indices, "FMI3 implicit output indices")?;
    let mut program_for_output = BTreeMap::new();
    let mut cursor = 0usize;

    for (program_index, program) in programs.iter().enumerate() {
        let row = program
            .downcast_object_ref::<SolveRowValue>()
            .ok_or_else(|| {
                render_err("FMI3 projection scheduling requires typed scalar Solve-IR rows")
            })?;
        let output_count = rumoca_ir_solve::ScalarProgramBlock::program_output_count(row.ops());
        let end = cursor
            .checked_add(output_count)
            .ok_or_else(|| render_err("FMI3 implicit output cursor overflow"))?;
        let indices = output_indices
            .get(cursor..end)
            .ok_or_else(|| render_err("FMI3 implicit output metadata is incomplete"))?;
        if output_count != 1 && indices.iter().any(|index| *index >= state_count) {
            return Ok(empty_schedule());
        }
        if let [output] = indices
            && program_for_output.insert(*output, program_index).is_some()
        {
            return Err(render_err(format!(
                "FMI3 implicit output row {output} has multiple scalar producers"
            )));
        }
        cursor = end;
    }
    if cursor != output_indices.len() {
        return Err(render_err(
            "FMI3 implicit output metadata has unused indices",
        ));
    }

    let mut schedule = Vec::new();
    schedule
        .try_reserve_exact(projection_rows.len())
        .map_err(|_| render_err("FMI3 projection schedule exceeds host memory limits"))?;
    for (row_index, y_index) in projection_rows {
        let program_index = *program_for_output.get(&row_index).ok_or_else(|| {
            render_err(format!(
                "FMI3 projection row {row_index} has no scalar producer"
            ))
        })?;
        let row = programs[program_index]
            .downcast_object_ref::<SolveRowValue>()
            .ok_or_else(|| render_err("FMI3 projection row lost its typed representation"))?;
        if !supports_assignment(row, y_index)? {
            return Ok(empty_schedule());
        }
        schedule.push(serde_json::json!({
            "program_index": program_index,
            "y_index": y_index,
        }));
    }
    Ok(Value::from_serialize(schedule))
}
