//! Expression / equation helpers shared by the structural lowering and the
//! structural diagnosis stages: duplicate-equation pruning, structural keys for
//! deduplication, and the compact `--trace` rendering.
//!
//! Duplicate pruning keys each continuous row on a span-insensitive semantic
//! fingerprint ([`rumoca_core::expression_semantic_fingerprint`]) bucketed in an
//! [`IndexMap`], and confirms every bucket hit with
//! [`rumoca_core::expressions_semantically_equal`] — the same pattern used by
//! `rumoca-phase-dae`'s runtime-precompute expression interning. The name
//! components of the key are exact interned [`VarName`] comparisons, so only the
//! expression component is a hash that needs confirming.
//!
//! What the key deliberately ignores: the row's own `span` and `origin`; the
//! resolution metadata on its `lhs` [`rumoca_core::Reference`] (component
//! structure, span, `DefId`, `generated` flag — only the interned [`VarName`]
//! survives into the key); and every span inside the rhs expression tree,
//! including the spans of literals, array elements, `if` branches and function
//! arguments.
//!
//! Exactly one thing is still provenance-sensitive, and it is narrower than it
//! sounds: for [`rumoca_core::Expression::FunctionCall`],
//! `expressions_semantically_equal` compares the *callee* `Reference` with
//! `Reference`'s `PartialEq`, which includes the component reference's span,
//! `DefId`, resolved-function metadata and `generated` flag. The call's
//! *arguments* are not provenance-sensitive — like every other operand they
//! recurse through `expressions_semantically_equal` and compare
//! span-insensitively. So two calls whose callees differ only in resolution
//! metadata share a fingerprint bucket, fail confirmation, and are both kept.
//! That is conservative (a bucket hit can only ever be rejected, never widened);
//! relaxing it would change the shared `rumoca-core` helper that also drives
//! structural elimination, so it does not belong in this pass.
//!
//! This key is intentionally *not* equivalent to the superseded rendered-`String`
//! key, which used `format!("{expr:?}")` for every node outside
//! `Binary`/`Unary`/`VarRef`/`BuiltinCall` and `format!("{:?}")` for the whole
//! `lhs`, making literals, call arguments, arrays and `if` rows separable by
//! source position alone. Two directions changed: rows that differ only in
//! provenance now collapse, and — because `scalar_count` is part of the key —
//! rows that agree on lhs and rhs but expand to a different number of scalar
//! residuals are now kept apart where the old key merged them.

use indexmap::IndexMap;
use rumoca_core::{Expression, VarName};
use rumoca_ir_dae as dae;

/// Span-insensitive identity used to prune duplicate continuous rows.
///
/// The name and shape components are exact; the `u64` members are semantic
/// fingerprints that must be confirmed inside their bucket.
#[derive(PartialEq, Eq, Hash)]
enum DuplicateEquationKey {
    /// `der(state) - residual` rows are identified by the differentiated state
    /// and the residual, never by the row's own lhs slot.
    DerivativeResidual {
        state: VarName,
        scalar_count: usize,
        residual: u64,
    },
    /// Every other row is identified by its lhs variable and its whole rhs.
    General {
        lhs: Option<VarName>,
        scalar_count: usize,
        rhs: u64,
    },
}

pub(super) fn remove_duplicate_continuous_equations(dae: &mut dae::Dae) {
    let equations = std::mem::take(&mut dae.continuous.equations);
    let mut unique: Vec<dae::Equation> = Vec::with_capacity(equations.len());
    let mut buckets: IndexMap<DuplicateEquationKey, Vec<usize>> = IndexMap::new();
    for equation in equations {
        let bucket = buckets
            .entry(duplicate_equation_key(&equation))
            .or_default();
        if bucket_holds_duplicate(bucket, &unique, &equation) {
            continue;
        }
        bucket.push(unique.len());
        unique.push(equation);
    }
    dae.continuous.equations = unique;
}

fn duplicate_equation_key(equation: &dae::Equation) -> DuplicateEquationKey {
    match derivative_residual_signature(&equation.rhs) {
        Some((state, residual)) => DuplicateEquationKey::DerivativeResidual {
            state: state.clone(),
            scalar_count: equation.scalar_count,
            residual: rumoca_core::expression_semantic_fingerprint(residual),
        },
        None => DuplicateEquationKey::General {
            lhs: equation.lhs.as_ref().map(|lhs| lhs.var_name().clone()),
            scalar_count: equation.scalar_count,
            rhs: rumoca_core::expression_semantic_fingerprint(&equation.rhs),
        },
    }
}

#[cfg(test)]
thread_local! {
    /// Test-only tally of the full semantic-equality confirmations performed
    /// below, so the scaling regression test can assert the comparison budget
    /// exactly instead of timing the run. Thread-local: pruning is synchronous
    /// on its caller's thread, so parallel tests never share this counter.
    static SEMANTIC_EQUALITY_CONFIRMATIONS: std::cell::Cell<usize> =
        const { std::cell::Cell::new(0) };
}

#[cfg(test)]
fn record_semantic_equality_confirmation() {
    SEMANTIC_EQUALITY_CONFIRMATIONS.with(|count| count.set(count.get() + 1));
}

/// Confirm a fingerprint-bucket hit with full semantic equality.
fn bucket_holds_duplicate(
    bucket: &[usize],
    unique: &[dae::Equation],
    candidate: &dae::Equation,
) -> bool {
    let candidate_identity = duplicate_identity_expression(candidate);
    bucket.iter().any(|index| {
        #[cfg(test)]
        record_semantic_equality_confirmation();
        rumoca_core::expressions_semantically_equal(
            duplicate_identity_expression(&unique[*index]),
            candidate_identity,
        )
    })
}

/// The sub-expression carrying a row's identity: the residual for
/// `der(state) - residual` rows, the whole rhs otherwise. Two rows only share a
/// bucket when they agree on this shape, so the arms never mix.
fn duplicate_identity_expression(equation: &dae::Equation) -> &Expression {
    match derivative_residual_signature(&equation.rhs) {
        Some((_, residual)) => residual,
        None => &equation.rhs,
    }
}

fn derivative_residual_signature(expr: &Expression) -> Option<(&VarName, &Expression)> {
    let Expression::Binary { op, lhs, rhs, .. } = expr else {
        return None;
    };
    if !matches!(op, rumoca_core::OpBinary::Sub) {
        return None;
    }
    derivative_target_var_name(lhs).map(|target| (target, rhs.as_ref()))
}

fn derivative_target_var_name(expr: &Expression) -> Option<&VarName> {
    let Expression::BuiltinCall { function, args, .. } = expr else {
        return None;
    };
    if *function != rumoca_core::BuiltinFunction::Der {
        return None;
    }
    let Expression::VarRef {
        name, subscripts, ..
    } = args.first()?
    else {
        return None;
    };
    if !subscripts.is_empty() {
        return None;
    }
    Some(name.var_name())
}

pub(super) fn equation_lhs_prefix(eq: &dae::Equation) -> String {
    match eq.lhs.as_ref() {
        Some(lhs) => format!("{} = ", lhs.as_str()),
        None => String::new(),
    }
}

/// Compact Modelica-ish rendering for `--trace` diagnostics only.
pub(super) fn debug_render_expr(expr: &rumoca_core::Expression) -> String {
    use rumoca_core::Expression as E;
    match expr {
        E::Literal { value, .. } => format!("{value:?}"),
        E::VarRef {
            name, subscripts, ..
        } => {
            if subscripts.is_empty() {
                name.as_str().to_string()
            } else {
                format!("{}[{} subs]", name.as_str(), subscripts.len())
            }
        }
        E::Binary { op, lhs, rhs, .. } => format!(
            "({} {op:?} {})",
            debug_render_expr(lhs),
            debug_render_expr(rhs)
        ),
        E::Unary { op, rhs, .. } => format!("({op:?} {})", debug_render_expr(rhs)),
        E::BuiltinCall { function, args, .. } => format!(
            "{function:?}({})",
            args.iter()
                .map(debug_render_expr)
                .collect::<Vec<_>>()
                .join(", ")
        ),
        E::FunctionCall { name, args, .. } => format!(
            "{}({})",
            name.as_str(),
            args.iter()
                .map(debug_render_expr)
                .collect::<Vec<_>>()
                .join(", ")
        ),
        E::If {
            branches,
            else_branch,
            ..
        } => format!(
            "if({} branches, else {})",
            branches.len(),
            debug_render_expr(else_branch)
        ),
        other => format!("<{}>", std::any::type_name_of_val(other)),
    }
}

#[cfg(test)]
mod tests;
