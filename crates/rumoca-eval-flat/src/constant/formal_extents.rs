//! Declared-extent obligations of array function formals.
//!
//! A function input declared with an unspecified dimension (`input Real x[:]`)
//! takes that extent from the actual argument (MLS §12.4.5 and §10.3.1), so
//! the only obligation such a dimension places on the call is rank. Every
//! other written dimension is an exact obligation: a literal (`Real x[3]`)
//! and an expression over earlier formals (`Real x[n]`) both name one extent
//! the actual must have, even when the expression evaluates to zero, because
//! `Real x[n]` with `n = 0` accepts only an empty vector.
//!
//! Both interpreters that bind a call frame (the flatten-phase shape frame
//! and the constant-value function evaluator) settle the written dimensions
//! to integers before consulting this module, so the obligation kinds are
//! decided here once from the formal's retained `shape_expr` and the settled
//! values, never from the values alone: a settled `0` is an exact obligation
//! or an unspecified one depending on what the source wrote.

use super::errors::EvalError;
use rumoca_core::{FunctionParam, Subscript};

/// One declared dimension of an array formal after its extent is settled.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum DeclaredExtent {
    /// The actual must have exactly this extent in this dimension.
    Exact(i64),
    /// The dimension was written as `:` (or the type carries no extent), so
    /// the actual's extent is taken as the bound size.
    Unspecified,
}

/// Classify the settled dimensions of `param`.
///
/// `settled` is the per-dimension integer the caller produced from
/// `shape_expr` (a literal index, an evaluated expression, or the retained
/// sentinel for `:`), one entry per declared dimension.
///
/// Flat retains the written subscripts in `shape_expr` whenever the source
/// wrote any, and the `:` subscript survives lowering as `Subscript::Colon`,
/// so a written shape is classified from the subscripts. When no written
/// shape is retained the dimensions come from a resolved literal shape or a
/// dimensioned type alias, and Flat stores `0` for a dimension that has no
/// extent there; that sentinel is the only remaining signal, so it is read
/// as unspecified.
pub(crate) fn declared_extents(param: &FunctionParam, settled: &[i64]) -> Vec<DeclaredExtent> {
    let written = param.shape_expr.len() == param.dimensions().len();
    settled
        .iter()
        .enumerate()
        .map(|(index, &extent)| {
            let unspecified = if written {
                matches!(param.shape_expr[index], Subscript::Colon { .. })
            } else {
                extent == 0
            };
            if unspecified {
                DeclaredExtent::Unspecified
            } else {
                DeclaredExtent::Exact(extent)
            }
        })
        .collect()
}

/// Bind the actual's checked shape to the formal's declared obligations.
///
/// Returns the extents the formal is bound with for the rest of the call: an
/// exact dimension keeps its declared extent (equal to the actual's) and an
/// unspecified dimension takes the actual's, so `size(x, k)` inside the
/// function reads the size the caller passed. Rank is always an exact
/// obligation: a scalar against `x[:]` and a vector against `x[:, :]` are
/// refused, since an unspecified extent still declares a dimension.
pub(crate) fn bind_formal_extents(
    param: &FunctionParam,
    settled: &[i64],
    actual: &[i64],
) -> Result<Vec<i64>, EvalError> {
    let declared = declared_extents(param, settled);
    let conforms = actual.len() == declared.len()
        && declared
            .iter()
            .zip(actual)
            .all(|(declared, actual)| match declared {
                DeclaredExtent::Exact(extent) => extent == actual,
                DeclaredExtent::Unspecified => true,
            });
    if !conforms {
        return Err(EvalError::function_error(
            format!(
                "argument `{}` has shape {actual:?}, expected {}",
                param.name,
                render_declared(&declared)
            ),
            param.span,
        ));
    }
    Ok(declared
        .iter()
        .zip(actual)
        .map(|(declared, actual)| match declared {
            DeclaredExtent::Exact(extent) => *extent,
            DeclaredExtent::Unspecified => *actual,
        })
        .collect())
}

/// Render declared obligations the way the source writes them, so a refusal
/// of `x[:, :]` reads `[:, :]` rather than the retained sentinel.
fn render_declared(declared: &[DeclaredExtent]) -> String {
    let parts: Vec<String> = declared
        .iter()
        .map(|extent| match extent {
            DeclaredExtent::Exact(extent) => extent.to_string(),
            DeclaredExtent::Unspecified => ":".to_string(),
        })
        .collect();
    format!("[{}]", parts.join(", "))
}
