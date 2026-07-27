pub(crate) fn filled_f64_values(
    len: usize,
    value: f64,
    context: &'static str,
) -> Result<Vec<f64>, String> {
    let mut values = vec_with_capacity(len, context)?;
    values.resize(len, value);
    Ok(values)
}

pub(crate) fn vec_with_capacity<T>(
    capacity: usize,
    context: &'static str,
) -> Result<Vec<T>, String> {
    let mut values = Vec::new();
    values
        .try_reserve_exact(capacity)
        .map_err(|_| format!("{context} capacity overflows"))?;
    Ok(values)
}
