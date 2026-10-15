use crate::RuntimeSolveError;
use indexmap::IndexMap;
use rumoca_ir_solve as solve;
use rustc_hash::FxHashMap;
use std::hash::Hash;
use std::rc::Rc;

#[cfg(test)]
use super::selected_programs::CompleteExpression;
use super::selected_programs::CompleteJacobian;

use super::{EvalSolveError, SolveExecutionBackend};

pub(super) fn required_compiled<T>(
    label: &str,
    source_spans: &[rumoca_core::Span],
    result: Result<T, String>,
) -> Result<T, EvalSolveError> {
    result.map_err(|message| EvalSolveError::CompiledExecution {
        message: format!("required compiled program `{label}` could not be lowered: {message}"),
        span: unique_span(source_spans),
    })
}

pub(super) fn optional_compiled<T>(label: &str, result: Result<T, String>) -> Option<T> {
    match result {
        Ok(compiled) => Some(compiled),
        Err(error) => {
            tracing::debug!(
                target: "rumoca_solver::native_execution",
                label,
                %error,
                "optional compiled backend unavailable"
            );
            None
        }
    }
}

/// `block` compiled by `backend` as a whole-block expression, if it compiles.
#[cfg(test)]
pub(super) fn compiled_expression(
    backend: Option<&Rc<dyn SolveExecutionBackend>>,
    label: &str,
    block: &solve::ScalarProgramBlock,
) -> Result<Option<Rc<CompleteExpression>>, EvalSolveError> {
    backend
        .map(|backend| {
            let whole = required_compiled(
                label,
                block.program_spans(),
                backend.compile_expression(block),
            )?;
            super::selected_programs::expression_from_whole(backend.as_ref(), label, block, whole)
        })
        .transpose()
}

/// `block` compiled by `backend` as a forward-mode Jacobian, if it compiles.
pub(super) fn compiled_jacobian(
    backend: Option<&Rc<dyn SolveExecutionBackend>>,
    label: &str,
    block: &solve::ScalarProgramBlock,
) -> Result<Option<Rc<CompleteJacobian>>, EvalSolveError> {
    backend
        .map(|backend| super::selected_programs::jacobian(backend.as_ref(), label, block))
        .transpose()
}

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
        .into_iter()
        .enumerate()
        .map(|(idx, name)| (name, idx))
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

pub(super) fn admitted_compiled_error(
    context: &'static str,
    source_spans: &[rumoca_core::Span],
    error: RuntimeSolveError,
) -> RuntimeSolveError {
    match error {
        RuntimeSolveError::CompiledExecution {
            message,
            source_spans: issued,
            ..
        } => RuntimeSolveError::CompiledExecution {
            context,
            message,
            source_spans: if issued.is_empty() {
                source_spans.into()
            } else {
                issued
            },
        },
        RuntimeSolveError::SolveIr { message, span } => RuntimeSolveError::SolveIr {
            message,
            span: span.or_else(|| unique_span(source_spans)),
        },
        RuntimeSolveError::SourceFault { message, span } => RuntimeSolveError::SourceFault {
            message,
            span: span.or_else(|| unique_span(source_spans)),
        },
        error => error,
    }
}

fn unique_span(spans: &[rumoca_core::Span]) -> Option<rumoca_core::Span> {
    let first = spans.first().copied()?;
    spans.iter().all(|span| *span == first).then_some(first)
}

pub(super) fn admitted_output_call(
    scratch: &std::cell::RefCell<Vec<f64>>,
    out: &mut [f64],
    call: impl FnOnce(&mut [f64]) -> Result<(), RuntimeSolveError>,
) -> Result<(), RuntimeSolveError> {
    let mut scratch = scratch.borrow_mut();
    resize_runtime_values(&mut scratch, out.len(), 0.0, "compiled output transaction")?;
    call(&mut scratch)?;
    out.copy_from_slice(&scratch);
    Ok(())
}
