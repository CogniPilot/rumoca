use std::collections::BTreeMap;

use minijinja::Value;

use crate::errors::render_err;

use super::render_expr::get_field;
use super::render_solve::SolveRowValue;

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

fn validate_assignment(row: &SolveRowValue, target: usize) -> Result<(), minijinja::Error> {
    let ops = row.ops();
    match rumoca_eval_solve::target_assignment_shape(ops)
        .map_err(|error| render_err(format!("invalid FMI3 target-assignment row: {error}")))?
    {
        Some(shape) if shape.target_y_index() == target => Ok(()),
        Some(shape) => Err(render_err(format!(
            "FMI3 projection row targets y[{}], not y[{target}]",
            shape.target_y_index()
        ))),
        None if !ops.iter().any(
            |op| matches!(op, rumoca_ir_solve::LinearOp::LoadY { index, .. } if *index == target),
        ) =>
        {
            Ok(())
        }
        None => Err(render_err(format!(
            "FMI3 projection row cannot directly assign y[{target}]"
        ))),
    }
}

pub(super) fn fmi3_scalar_projection_schedule_function(
    blocks: Value,
    programs: Value,
    output_indices: Value,
    state_count: Value,
) -> Result<Value, minijinja::Error> {
    let state_count = state_count
        .as_usize()
        .ok_or_else(|| render_err("FMI3 Solve-IR state count must be a non-negative integer"))?;
    let blocks = blocks
        .try_iter()
        .map_err(|_| render_err("FMI3 algebraic projection blocks must be a sequence"))?
        .collect::<Vec<_>>();
    if blocks.is_empty() {
        return Ok(Value::from_serialize(Vec::<serde_json::Value>::new()));
    }
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
            return Err(render_err(
                "FMI3 scalar projection does not support multi-output algebraic programs",
            ));
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
        .try_reserve_exact(blocks.len())
        .map_err(|_| render_err("FMI3 projection schedule exceeds host memory limits"))?;
    for block in &blocks {
        let rows = required_sequence_field(block, "rows")?;
        let y_indices = required_sequence_field(block, "y_indices")?;
        let causal_steps = required_sequence_field(block, "causal_steps")?;
        if rows.len() != 1 || y_indices.len() != 1 || !causal_steps.is_empty() {
            return Err(render_err(
                "FMI3 C generation requires scalar projection blocks without causal steps",
            ));
        }
        let row_index = rows[0]
            .as_usize()
            .ok_or_else(|| render_err("FMI3 projection row must be a non-negative integer"))?;
        let y_index = y_indices[0]
            .as_usize()
            .ok_or_else(|| render_err("FMI3 projection target must be a non-negative integer"))?;
        let program_index = *program_for_output.get(&row_index).ok_or_else(|| {
            render_err(format!(
                "FMI3 projection row {row_index} has no scalar producer"
            ))
        })?;
        let row = programs[program_index]
            .downcast_object_ref::<SolveRowValue>()
            .ok_or_else(|| render_err("FMI3 projection row lost its typed representation"))?;
        validate_assignment(row, y_index)?;
        schedule.push(serde_json::json!({
            "program_index": program_index,
            "y_index": y_index,
        }));
    }
    Ok(Value::from_serialize(schedule))
}
