//! Inline node budget and memoization for function-call projection.
//!
//! Textual inlining duplicates argument expressions per use, which grows
//! exponentially across nested array-valued calls without a size cutoff.
//! Exceeding the budget is a typed `LowerError::ProjectionBudgetExceeded`
//! that propagates like any other error; the only place allowed to recover
//! it is `top_level_function_call_outputs`, where keeping the original
//! runtime call is the semantically equivalent lowering.

use super::*;
use rumoca_core::FallibleExpressionVisitor;

pub(super) struct CallOutputsCacheEntry {
    pub(super) call: rumoca_core::Expression,
    pub(super) depth: usize,
    pub(super) outcome: CachedProjectionOutcome,
}

/// Deterministic projection outcomes worth memoizing: successful projections
/// (including "not projectable here" `None`) and budget declines. Other
/// errors abort the whole lowering, so they are never re-asked.
#[derive(Clone)]
pub(super) enum CachedProjectionOutcome {
    Outputs(Option<Vec<ProjectedFunctionOutput>>),
    BudgetExceeded {
        function: String,
        span: rumoca_core::Span,
    },
}

impl CachedProjectionOutcome {
    pub(super) fn into_result(self) -> Result<Option<Vec<ProjectedFunctionOutput>>, LowerError> {
        match self {
            Self::Outputs(outputs) => Ok(outputs),
            Self::BudgetExceeded { function, span } => {
                Err(LowerError::ProjectionBudgetExceeded { function, span })
            }
        }
    }
}

pub(super) fn projection_budget_exceeded(function: &rumoca_core::Function) -> LowerError {
    LowerError::ProjectionBudgetExceeded {
        function: function.name.to_string(),
        span: function.span,
    }
}

/// True when `expr` has more nodes than `MAX_FUNCTION_PROJECTION_NODES`;
/// stops counting at the budget instead of walking the whole tree.
pub(super) fn exceeds_projection_node_budget(expr: &rumoca_core::Expression) -> bool {
    struct NodeBudget {
        remaining: usize,
    }
    impl FallibleExpressionVisitor for NodeBudget {
        type Error = ();
        fn visit_expression(&mut self, expr: &rumoca_core::Expression) -> Result<(), ()> {
            self.remaining = self.remaining.checked_sub(1).ok_or(())?;
            self.walk_expression(expr)
        }
    }
    NodeBudget {
        remaining: crate::lower::MAX_FUNCTION_PROJECTION_NODES,
    }
    .visit_expression(expr)
    .is_err()
}

impl FunctionProjectionAnalysis<'_> {
    /// Outermost projection boundary. A budget-exceeded projection resolves
    /// to `None` here so the caller keeps the original runtime call (the
    /// lowering every non-projected function already uses); all other errors
    /// propagate.
    pub(super) fn top_level_function_call_outputs(
        &self,
        expr: &rumoca_core::Expression,
        owner_span: rumoca_core::Span,
    ) -> Result<Option<Vec<ProjectedFunctionOutput>>, LowerError> {
        match self.function_call_outputs_with_owner(expr, 0, owner_span) {
            Err(err) if err.is_projection_budget_exceeded() => Ok(None),
            other => other,
        }
    }

    pub(super) fn cached_call_outputs(
        &self,
        expr: &rumoca_core::Expression,
        depth: usize,
    ) -> Option<CachedProjectionOutcome> {
        self.call_outputs_cache
            .borrow()
            .iter()
            .find(|entry| entry.depth == depth && entry.call == *expr)
            .map(|entry| entry.outcome.clone())
    }

    pub(super) fn record_call_outputs_outcome(
        &self,
        expr: &rumoca_core::Expression,
        depth: usize,
        outcome: CachedProjectionOutcome,
    ) {
        self.call_outputs_cache
            .borrow_mut()
            .push(CallOutputsCacheEntry {
                call: expr.clone(),
                depth,
                outcome,
            });
    }
}
