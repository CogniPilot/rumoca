//! Placeholder interiors of structured families whose template is their owner
//! (SPEC_0043 §6c): which owner reads the template, and which cells keep full
//! bodies.

use super::*;

/// Per-binder base and optional `+step` neighbor values used to identify a regular
/// family's corner cells during materialization. A cell is a corner when its index
/// tuple equals the base, or differs from the base in exactly one binder, at that
/// binder's neighbor value.
pub(super) struct CheapenPlan {
    binders: Vec<(i64, Option<i64>)>,
}

impl CheapenPlan {
    pub(super) fn is_corner(&self, index_values: &[i64]) -> bool {
        let mut differing = false;
        for (&(base, neighbor), &value) in self.binders.iter().zip(index_values) {
            if value == base {
                continue;
            }
            if differing || neighbor != Some(value) {
                return false;
            }
            differing = true;
        }
        true
    }
}

/// True when every equation in the body is a state-derivative assignment
/// `der(x[...]) = ...`. Only these are safe to cheapen: solve reconstructs the
/// derivative from the corner stencil at runtime, so the interior bodies are never
/// read. Algebraic assignments are excluded -- their per-cell values feed
/// compile-time derived-parameter promotion.
/// Which owner reads a family's template instead of its cells, or
/// `Materialized` when no owner can (SPEC_0043 §6c).
pub(super) fn placeholder_interiors(
    ctx: &Context,
    indices: &[ast::ForIndex],
    equations: &[ast::Equation],
    regular: bool,
    template: Option<&rumoca_core::ComprehensionTemplate>,
) -> flat::FamilyInteriors {
    let Some(template) = template.filter(|_| !ctx.materialize_structured_families) else {
        return flat::FamilyInteriors::Materialized;
    };
    if regular && is_state_derivative_body(equations) {
        return flat::FamilyInteriors::StateDerivative;
    }
    let Some(owner) = ctx.current_class_instance_id else {
        return flat::FamilyInteriors::Materialized;
    };
    if regular
        && crate::param_variability::is_proven_parameter_variability_assignment_body(
            owner,
            indices,
            equations,
            &ctx.param_variability_families,
        )
    {
        flat::FamilyInteriors::ParameterAssignment
    } else if ctx
        .continuous_algebraic_targets
        .admits(owner, indices, equations, template)
    {
        flat::FamilyInteriors::ContinuousAlgebraic
    } else {
        flat::FamilyInteriors::Materialized
    }
}

fn is_state_derivative_body(equations: &[ast::Equation]) -> bool {
    !equations.is_empty()
        && equations.iter().all(|equation| match equation {
            ast::Equation::Simple { lhs, .. } => is_der_call(lhs),
            _ => false,
        })
}

/// True when `expr` is a `der(...)` call.
fn is_der_call(expr: &ast::Expression) -> bool {
    matches!(
        expr,
        ast::Expression::FunctionCall { comp, .. }
            if comp.parts.len() == 1 && comp.parts[0].ident.text.as_ref() == "der"
    )
}

/// Build the corner predicate for a regular for-family by expanding each binder's
/// range to read its base (first) and neighbor (second) values. `None` when any
/// binder range is empty (the family has no cells, so nothing to cheapen).
pub(super) fn build_cheapen_plan(
    ctx: &Context,
    indices: &[ast::ForIndex],
    prefix: &ast::QualifiedName,
    span: rumoca_core::Span,
) -> Result<Option<CheapenPlan>, FlattenError> {
    let mut binders = Vec::with_capacity(indices.len());
    for index in indices {
        let values = expand_range_indices(ctx, &index.range, prefix, span)?;
        let Some(&base) = values.first() else {
            return Ok(None);
        };
        binders.push((base, values.get(1).copied()));
    }
    Ok(Some(CheapenPlan { binders }))
}

/// Replace each `Simple` equation's right-hand side with a real `0.0` literal,
/// keeping the left-hand side, through nested `for` equations. Used for the
/// non-corner cells of a family whose template owns its body (SPEC_0043 §6c):
/// a nested loop's cells are cells of the enclosing family, so they are
/// placeholders too and the full bodies stay at the corners. Other equations
/// are left unchanged.
pub(super) fn cheapen_equation_bodies(
    equations: &[ast::Equation],
    span: rumoca_core::Span,
) -> Vec<ast::Equation> {
    equations
        .iter()
        .map(|equation| match equation {
            ast::Equation::Simple { lhs, .. } => ast::Equation::Simple {
                lhs: lhs.clone(),
                rhs: zero_sized_reductions::real_literal_expr(0.0, span),
            },
            ast::Equation::For { indices, equations } => ast::Equation::For {
                indices: indices.clone(),
                equations: cheapen_equation_bodies(equations, span),
            },
            other => other.clone(),
        })
        .collect()
}
