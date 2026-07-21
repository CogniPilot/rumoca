use indexmap::IndexMap;
use rumoca_solver::RuntimeSolveError;
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
