use super::{CompileError, LinearOp, RowPlan};

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

pub(super) fn input_requirements_for_linear_ops(
    row: &[LinearOp],
) -> Result<InputRequirements, CompileError> {
    row.iter()
        .copied()
        .map(input_requirements_for_linear_op)
        .try_fold(InputRequirements::default(), |requirements, op| {
            op.map(|op_requirements| requirements.merge(op_requirements))
        })
}

fn input_requirements_for_linear_op(op: LinearOp) -> Result<InputRequirements, CompileError> {
    match op {
        LinearOp::LoadY { index, .. } => Ok(InputRequirements {
            y_len: checked_required_len("y", index)?,
            ..Default::default()
        }),
        LinearOp::LoadP { index, .. } => Ok(InputRequirements {
            p_len: checked_required_len("p", index)?,
            ..Default::default()
        }),
        LinearOp::LoadIndexedP { base, count, .. } => Ok(InputRequirements {
            p_len: checked_required_indexed_len("p", base, count)?,
            ..Default::default()
        }),
        LinearOp::LoadSeed { index, .. } => Ok(InputRequirements {
            seed_len: checked_required_len("seed", index)?,
            ..Default::default()
        }),
        LinearOp::LoadIndexedSeed { base, count, .. } => Ok(InputRequirements {
            seed_len: checked_required_indexed_len("seed", base, count)?,
            ..Default::default()
        }),
        _ => Ok(InputRequirements::default()),
    }
}

fn checked_required_len(vector: &'static str, index: usize) -> Result<usize, CompileError> {
    index
        .checked_add(1)
        .ok_or_else(|| CompileError::Backend(format!("{vector} input requirement overflow")))
}

fn checked_required_indexed_len(
    vector: &'static str,
    base: usize,
    count: usize,
) -> Result<usize, CompileError> {
    if count == 0 {
        return checked_required_len(vector, base);
    }
    base.checked_add(count)
        .ok_or_else(|| CompileError::Backend(format!("{vector} input requirement overflow")))
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
