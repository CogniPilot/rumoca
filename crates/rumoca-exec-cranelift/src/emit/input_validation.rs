use super::{CompileError, LinearOp, RowPlan, input_compile_error};

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub(crate) struct InputRequirements {
    pub(crate) y_len: usize,
    pub(crate) p_len: usize,
    pub(crate) seed_len: usize,
}

impl InputRequirements {
    fn merge(self, other: Self) -> Self {
        Self {
            y_len: self.y_len.max(other.y_len),
            p_len: self.p_len.max(other.p_len),
            seed_len: self.seed_len.max(other.seed_len),
        }
    }
}

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

pub(super) fn input_requirements_for_linear_ops(row: &[LinearOp]) -> InputRequirements {
    row.iter()
        .copied()
        .map(input_requirements_for_linear_op)
        .fold(InputRequirements::default(), InputRequirements::merge)
}

fn input_requirements_for_linear_op(op: LinearOp) -> InputRequirements {
    match op {
        LinearOp::LoadY { index, .. } => InputRequirements {
            y_len: index.saturating_add(1),
            ..Default::default()
        },
        LinearOp::LoadP { index, .. } => InputRequirements {
            p_len: index.saturating_add(1),
            ..Default::default()
        },
        LinearOp::LoadSeed { index, .. } => InputRequirements {
            seed_len: index.saturating_add(1),
            ..Default::default()
        },
        _ => InputRequirements::default(),
    }
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
    Err(input_compile_error(
        vector,
        required_len.saturating_sub(1),
        actual_len,
    ))
}
