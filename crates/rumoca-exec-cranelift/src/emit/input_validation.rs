use super::{CompileError, LinearOp, RowPlan};

pub(crate) use rumoca_eval_solve::RowInputRequirements as InputRequirements;

#[cfg(test)]
mod tests;

pub(super) fn input_requirements_for_plans(rows: &[RowPlan]) -> InputRequirements {
    rows.iter()
        .map(row_input_requirements)
        .fold(InputRequirements::default(), InputRequirements::merge)
}

pub(super) fn row_input_requirements(row: &RowPlan) -> InputRequirements {
    match row {
        RowPlan::Simple(row) => row.input_requirements,
        RowPlan::General(row) => row.input_requirements,
    }
}

pub(super) fn input_requirements_for_linear_ops(
    row: &[LinearOp],
) -> Result<InputRequirements, CompileError> {
    rumoca_eval_solve::row_input_requirements(row)
        .map_err(|error| CompileError::Backend(error.to_string()))
}

pub(super) fn validate_output_len(out: &[f64], row_count: usize) -> Result<(), CompileError> {
    if out.len() >= row_count {
        return Ok(());
    }
    Err(CompileError::Input(format!(
        "output buffer too small: {} < {}",
        out.len(),
        row_count
    )))
}

pub(super) fn validate_input_requirements(
    requirements: InputRequirements,
    y: &[f64],
    p: &[f64],
    seed: Option<&[f64]>,
) -> Result<(), CompileError> {
    validate_input_len("y", y.len(), requirements.y_len)?;
    validate_input_len("p", p.len(), requirements.p_len)?;
    if requirements.seed_len == 0 {
        return Ok(());
    }
    validate_input_len(
        "seed",
        seed.map_or(0, |values| values.len()),
        requirements.seed_len,
    )
}

fn validate_input_len(
    vector: &'static str,
    actual_len: usize,
    required_len: usize,
) -> Result<(), CompileError> {
    if actual_len >= required_len {
        return Ok(());
    }
    Err(input_compile_error(vector, required_len - 1, actual_len))
}

pub(super) fn input_compile_error(vector: &'static str, index: usize, len: usize) -> CompileError {
    CompileError::Input(format!(
        "missing {vector}[{index}] while evaluating Cranelift row; vector length is {len}"
    ))
}
