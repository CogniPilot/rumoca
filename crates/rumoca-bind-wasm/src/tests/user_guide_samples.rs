//! Shared observers for the guide's actual JSON numerical front doors.
#![cfg(any(feature = "sim-wasm", feature = "sim-diffsol", feature = "sim-rk45"))]

use serde_json::Value;

pub(super) fn finite_vector(value: &Value, len: usize) -> Result<(), &'static str> {
    let values = value.as_array().ok_or("missing numeric vector")?;
    if values.len() != len {
        return Err("numeric vector has the wrong extent");
    }
    if values
        .iter()
        .any(|v| !v.as_f64().is_some_and(f64::is_finite))
    {
        return Err("numeric vector contains a nonfinite or nonnumeric cell");
    }
    Ok(())
}

pub(super) fn finite_sample_grid(result: &Value, expected: &[f64]) -> Result<(), &'static str> {
    let columns = result
        .pointer("/payload/allData")
        .and_then(Value::as_array)
        .ok_or("missing sample columns")?;
    if columns.len() < 2 || expected.is_empty() {
        return Err("missing time or numerical result column");
    }
    let times = columns[0].as_array().ok_or("missing sampled times")?;
    if times.len() < expected.len() {
        return Err("missing requested samples");
    }
    for column in columns {
        finite_vector(column, times.len())?;
    }
    let time = |v: &Value| v.as_f64().expect("finite_vector checked time cells");
    if times
        .windows(2)
        .any(|pair| time(&pair[0]) >= time(&pair[1]))
    {
        return Err("sample times are not strictly increasing");
    }
    let near = |a: f64, b: f64| (a - b).abs() <= 1e-12;
    if !near(time(&times[0]), expected[0])
        || !near(time(times.last().unwrap()), *expected.last().unwrap())
    {
        return Err("sample trace does not cover the requested endpoints");
    }
    if expected
        .iter()
        .any(|wanted| !times.iter().any(|actual| near(time(actual), *wanted)))
    {
        return Err("sample trace is missing a requested grid point");
    }
    Ok(())
}

#[test]
fn user_guide_numerical_observers_reject_incomplete_or_invalid_payloads() {
    use serde_json::json;
    let result = |columns: Value| json!({"payload": {"allData": columns}});
    let grid = [0.0, 0.01, 0.02];
    let valid = json!([[0.0, 0.01, 0.02], [1.0, 0.9, 0.8]]);
    assert!(finite_sample_grid(&result(valid), &grid).is_ok());
    // Extra event observations may occur between the requested output times.
    let events = json!([[0.0, 0.005, 0.01, 0.02], [1.0, 0.95, 0.9, 0.8]]);
    assert!(finite_sample_grid(&result(events), &grid).is_ok());
    for invalid in [
        json!([]),
        json!([[0.0, 0.01, 0.02]]),
        json!([[0.0, 0.01, 0.02], [1.0, 0.9]]),
        json!([[0.0, 0.01, 0.02], [1.0, null, 0.8]]),
        json!([[0.0, 0.02, 0.01], [1.0, 0.9, 0.8]]),
        json!([[0.0, 0.015, 0.02], [1.0, 0.9, 0.8]]),
        json!([[0.005, 0.01, 0.02], [1.0, 0.9, 0.8]]),
        json!([[0.0, 0.01, 0.015], [1.0, 0.9, 0.8]]),
    ] {
        assert!(finite_sample_grid(&result(invalid), &grid).is_err());
    }
    assert!(finite_vector(&json!([0.0, -0.0]), 2).is_ok());
    assert!(finite_vector(&json!([]), 0).is_ok());
    assert!(finite_vector(&json!([0.0]), 2).is_err());
    assert!(finite_vector(&json!([null]), 1).is_err());
}
