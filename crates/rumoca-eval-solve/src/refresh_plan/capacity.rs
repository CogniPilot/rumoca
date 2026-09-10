use std::collections::VecDeque;

use indexmap::{IndexMap, IndexSet};

use crate::EvalSolveError;

pub(super) fn reserve_refresh_vec_capacity<T>(
    values: &mut Vec<T>,
    capacity: usize,
    context: &'static str,
    span: Option<rumoca_core::Span>,
) -> Result<(), EvalSolveError> {
    values
        .try_reserve_exact(capacity)
        .map_err(|_| refresh_plan_capacity_error(context, span))
}

pub(super) fn reserve_refresh_deque_capacity<T>(
    values: &mut VecDeque<T>,
    capacity: usize,
    context: &'static str,
    span: Option<rumoca_core::Span>,
) -> Result<(), EvalSolveError> {
    values
        .try_reserve_exact(capacity)
        .map_err(|_| refresh_plan_capacity_error(context, span))
}

pub(super) fn reserve_refresh_index_map_capacity<K, V>(
    values: &mut IndexMap<K, V>,
    capacity: usize,
    context: &'static str,
    span: Option<rumoca_core::Span>,
) -> Result<(), EvalSolveError>
where
    K: std::hash::Hash + Eq,
{
    values
        .try_reserve(capacity)
        .map_err(|_| refresh_plan_capacity_error(context, span))
}

pub(super) fn reserve_refresh_index_set_capacity<T>(
    values: &mut IndexSet<T>,
    capacity: usize,
    context: &'static str,
    span: Option<rumoca_core::Span>,
) -> Result<(), EvalSolveError>
where
    T: std::hash::Hash + Eq,
{
    values
        .try_reserve(capacity)
        .map_err(|_| refresh_plan_capacity_error(context, span))
}

fn refresh_plan_capacity_error(
    context: &'static str,
    span: Option<rumoca_core::Span>,
) -> EvalSolveError {
    EvalSolveError::InvalidRow {
        message: format!("refresh plan {context} capacity overflows"),
        span,
    }
}
