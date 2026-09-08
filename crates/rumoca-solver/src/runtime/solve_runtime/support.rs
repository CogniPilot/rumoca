use crate::RuntimeSolveError;
use indexmap::IndexMap;
use rumoca_ir_solve as solve;
use rustc_hash::FxHashMap;
use std::hash::Hash;

pub(super) fn zero_runtime_values(
    len: usize,
    context: &'static str,
) -> Result<Vec<f64>, RuntimeSolveError> {
    let mut values = Vec::new();
    reserve_runtime_vec_capacity(&mut values, len, context)?;
    values.resize(len, 0.0);
    Ok(values)
}

pub(super) fn copy_runtime_values(
    values: &[f64],
    context: &'static str,
) -> Result<Vec<f64>, RuntimeSolveError> {
    let mut copy = Vec::new();
    reserve_runtime_vec_capacity(&mut copy, values.len(), context)?;
    copy.extend_from_slice(values);
    Ok(copy)
}

pub(super) fn copy_runtime_values_into(
    dst: &mut Vec<f64>,
    values: &[f64],
    context: &'static str,
) -> Result<(), RuntimeSolveError> {
    if dst.len() < values.len() {
        reserve_runtime_vec_capacity(dst, values.len() - dst.len(), context)?;
    }
    dst.clear();
    dst.extend_from_slice(values);
    Ok(())
}

pub(super) fn resize_runtime_values(
    values: &mut Vec<f64>,
    len: usize,
    value: f64,
    context: &'static str,
) -> Result<(), RuntimeSolveError> {
    if values.len() < len {
        reserve_runtime_vec_capacity(values, len - values.len(), context)?;
    }
    values.resize(len, value);
    Ok(())
}

pub(super) fn reserve_runtime_vec_capacity<T>(
    values: &mut Vec<T>,
    capacity: usize,
    context: &'static str,
) -> Result<(), RuntimeSolveError> {
    values
        .try_reserve_exact(capacity)
        .map_err(|_| RuntimeSolveError::solve_ir(format!("{context} capacity overflows")))
}

pub(super) fn reserve_runtime_index_map_capacity<K, V>(
    values: &mut IndexMap<K, V>,
    capacity: usize,
    context: &'static str,
) -> Result<(), RuntimeSolveError>
where
    K: Eq + Hash,
{
    values
        .try_reserve(capacity)
        .map_err(|_| RuntimeSolveError::solve_ir(format!("{context} capacity overflows")))
}

pub(super) fn build_visible_name_index(model: &solve::SolveModel) -> FxHashMap<String, usize> {
    model
        .visible_names()
        .enumerate()
        .map(|(idx, name)| (name.to_string(), idx))
        .collect()
}

pub(super) fn fill_inactive_root_output(out: &mut [f64]) -> Result<(), RuntimeSolveError> {
    if let Some(first) = out.first_mut() {
        *first = 1.0;
    }
    Ok(())
}

pub(super) fn validate_runtime_output_len(
    context: &str,
    expected: usize,
    actual: usize,
) -> Result<(), RuntimeSolveError> {
    if actual == expected {
        return Ok(());
    }
    Err(RuntimeSolveError::solve_ir(format!(
        "{context} expected {expected} values, got {actual}"
    )))
}

pub(super) fn validate_finite_runtime_output(
    context: &str,
    values: &[f64],
) -> Result<(), RuntimeSolveError> {
    if let Some((index, value)) = values
        .iter()
        .copied()
        .enumerate()
        .find(|(_, value)| !value.is_finite())
    {
        return Err(RuntimeSolveError::solve_ir(format!(
            "{context} produced non-finite value {value} at index {index}"
        )));
    }
    Ok(())
}

pub(super) fn visible_value_index_error(
    name: &str,
    index: usize,
    len: usize,
    context: &'static str,
) -> RuntimeSolveError {
    RuntimeSolveError::solve_ir(format!(
        "{context} for visible name `{name}` reference index {index}, but only {len} values are available"
    ))
}
