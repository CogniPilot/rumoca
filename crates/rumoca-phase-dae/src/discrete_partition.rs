//! Helpers for partitioning clocked/discrete equations in ToDAE.
//!
//! MLS Appendix B separates continuous equations (`f_x`) from discrete updates
//! (`f_z`/`f_m`). This module provides shared classification helpers so variable
//! and equation partitioning use the same rules.

use std::collections::HashSet;

use rumoca_ir_dae as dae;
use rumoca_ir_flat as flat;

use crate::flat_to_dae_var_name;
use crate::path_utils::subscript_fallback_chain;

/// Discrete bucket for equations with explicit LHS targets.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ResidualDiscreteBucket {
    DiscreteReal,
    DiscreteValued,
    Mixed,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum NameDiscreteBucket {
    DiscreteReal,
    DiscreteValued,
}

/// Returns true when an equation has an explicit assignment-form LHS and
/// contains clocked/event operators (`previous`, `pre`, `sample`, ...).
pub(crate) fn is_clocked_assignment_equation(eq: &flat::Equation) -> bool {
    !residual_lhs_targets(&eq.residual).is_empty()
        && expression_contains_clocked_or_event_operators(&eq.residual)
}

/// Collect assignment targets from residual form `lhs - rhs = 0`.
///
/// Supports tuple/array LHS by collecting all variable references in the LHS.
pub(crate) fn residual_lhs_targets(residual: &flat::Expression) -> Vec<flat::VarName> {
    let mut targets = Vec::new();
    match residual {
        flat::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(_),
            lhs,
            rhs,
        } => {
            collect_lhs_targets(lhs, &mut targets);
            extend_wrapped_assignment_targets(lhs, rhs, &mut targets);
        }
        // Some flattened if-equations are already in residual form without an
        // outer `0 - (...)` wrapper. Recover assignment targets from active
        // branch residuals so discrete partitioning remains spec-aligned.
        flat::Expression::If { .. }
        | flat::Expression::Unary {
            op: rumoca_ir_core::OpUnary::Minus(_),
            ..
        } => {
            let _ = collect_assignment_targets_from_residual_rhs(residual, &mut targets);
        }
        _ => {}
    }

    let mut seen = HashSet::new();
    targets.retain(|name| seen.insert(name.clone()));
    targets
}

fn extend_wrapped_assignment_targets(
    lhs: &flat::Expression,
    rhs: &flat::Expression,
    out: &mut Vec<flat::VarName>,
) {
    if !out.is_empty() {
        return;
    }
    if is_numeric_zero(lhs) {
        extend_assignment_targets(rhs, out);
        return;
    }
    if is_numeric_zero(rhs) {
        extend_assignment_targets(lhs, out);
    }
}

fn extend_assignment_targets(expr: &flat::Expression, out: &mut Vec<flat::VarName>) {
    let mut candidates = Vec::new();
    if collect_assignment_targets_from_residual_rhs(expr, &mut candidates) {
        out.extend(candidates);
    }
}

fn is_numeric_zero(expr: &flat::Expression) -> bool {
    match expr {
        flat::Expression::Literal(flat::Literal::Integer(0)) => true,
        flat::Expression::Literal(flat::Literal::Real(v)) => v.abs() <= f64::EPSILON,
        _ => false,
    }
}

fn collect_assignment_targets_from_residual_rhs(
    expr: &flat::Expression,
    out: &mut Vec<flat::VarName>,
) -> bool {
    match expr {
        flat::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(_),
            lhs,
            ..
        } => {
            let before = out.len();
            collect_lhs_targets(lhs, out);
            out.len() > before
        }
        flat::Expression::Unary {
            op: rumoca_ir_core::OpUnary::Minus(_),
            rhs,
        } => collect_assignment_targets_from_residual_rhs(rhs, out),
        flat::Expression::If {
            branches,
            else_branch,
        } => {
            if branches.is_empty() {
                return false;
            }
            let mut saw_target = false;
            for (_, value) in branches {
                saw_target |= collect_assignment_targets_from_residual_rhs(value, out);
            }
            saw_target |= collect_assignment_targets_from_residual_rhs(else_branch, out);
            saw_target
        }
        flat::Expression::Tuple { elements } | flat::Expression::Array { elements, .. } => {
            if elements.is_empty() {
                return false;
            }
            let mut saw_target = false;
            for element in elements {
                saw_target |= collect_assignment_targets_from_residual_rhs(element, out);
            }
            saw_target
        }
        _ => false,
    }
}

/// Classify a residual equation into a discrete bucket if all explicit LHS
/// targets are known discrete variables.
pub(crate) fn classify_residual_discrete_bucket(
    dae: &dae::Dae,
    residual: &flat::Expression,
) -> Option<ResidualDiscreteBucket> {
    let targets = residual_lhs_targets(residual);
    if targets.is_empty() {
        return None;
    }

    let mut saw_real = false;
    let mut saw_valued = false;
    for target in targets {
        match discrete_bucket_for_name(dae, &target) {
            Some(NameDiscreteBucket::DiscreteReal) => saw_real = true,
            Some(NameDiscreteBucket::DiscreteValued) => saw_valued = true,
            None => return None,
        }
    }

    match (saw_real, saw_valued) {
        (true, false) => Some(ResidualDiscreteBucket::DiscreteReal),
        (false, true) => Some(ResidualDiscreteBucket::DiscreteValued),
        (true, true) => Some(ResidualDiscreteBucket::Mixed),
        (false, false) => None,
    }
}

fn collect_lhs_targets(lhs: &flat::Expression, out: &mut Vec<flat::VarName>) {
    match lhs {
        flat::Expression::VarRef { name, .. } => out.push(name.clone()),
        flat::Expression::BuiltinCall {
            function: flat::BuiltinFunction::Der,
            args,
        } if args.len() == 1 => collect_lhs_targets(&args[0], out),
        flat::Expression::If {
            branches,
            else_branch,
        } => {
            for (_, value) in branches {
                collect_lhs_targets(value, out);
            }
            collect_lhs_targets(else_branch, out);
        }
        flat::Expression::Unary { rhs, .. } => collect_lhs_targets(rhs, out),
        flat::Expression::Index { base, subscripts } => {
            collect_lhs_targets(base, out);
            for sub in subscripts {
                if let flat::Subscript::Expr(expr) = sub {
                    collect_lhs_targets(expr, out);
                }
            }
        }
        flat::Expression::FieldAccess { base, .. } => collect_lhs_targets(base, out),
        flat::Expression::Tuple { elements } | flat::Expression::Array { elements, .. } => {
            for element in elements {
                collect_lhs_targets(element, out);
            }
        }
        _ => {}
    }
}

fn discrete_bucket_for_name(dae: &dae::Dae, name: &flat::VarName) -> Option<NameDiscreteBucket> {
    if dae
        .discrete_valued
        .contains_key(&flat_to_dae_var_name(name))
        || subscript_fallback_chain(name).into_iter().any(|candidate| {
            dae.discrete_valued
                .contains_key(&flat_to_dae_var_name(&candidate))
        })
    {
        return Some(NameDiscreteBucket::DiscreteValued);
    }
    if dae.discrete_reals.contains_key(&flat_to_dae_var_name(name))
        || subscript_fallback_chain(name).into_iter().any(|candidate| {
            dae.discrete_reals
                .contains_key(&flat_to_dae_var_name(&candidate))
        })
    {
        return Some(NameDiscreteBucket::DiscreteReal);
    }
    None
}

/// Returns true when expression contains clocked/event primitives that imply
/// event-driven evaluation.
pub(crate) fn expression_contains_clocked_or_event_operators(expr: &flat::Expression) -> bool {
    match expr {
        flat::Expression::BuiltinCall { function, args } => {
            if matches!(
                function,
                flat::BuiltinFunction::Pre
                    | flat::BuiltinFunction::Sample
                    | flat::BuiltinFunction::Edge
                    | flat::BuiltinFunction::Change
            ) {
                return true;
            }
            args.iter()
                .any(expression_contains_clocked_or_event_operators)
        }
        flat::Expression::FunctionCall { name, args, .. } => {
            let short_name = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
            if is_clock_intrinsic_short_name(short_name) {
                return true;
            }
            args.iter()
                .any(expression_contains_clocked_or_event_operators)
        }
        flat::Expression::Binary { lhs, rhs, .. } => {
            expression_contains_clocked_or_event_operators(lhs)
                || expression_contains_clocked_or_event_operators(rhs)
        }
        flat::Expression::Unary { rhs, .. } => expression_contains_clocked_or_event_operators(rhs),
        flat::Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(cond, value)| {
                expression_contains_clocked_or_event_operators(cond)
                    || expression_contains_clocked_or_event_operators(value)
            }) || expression_contains_clocked_or_event_operators(else_branch)
        }
        flat::Expression::Array { elements, .. } | flat::Expression::Tuple { elements } => elements
            .iter()
            .any(expression_contains_clocked_or_event_operators),
        flat::Expression::Range { start, step, end } => {
            expression_contains_clocked_or_event_operators(start)
                || step
                    .as_deref()
                    .is_some_and(expression_contains_clocked_or_event_operators)
                || expression_contains_clocked_or_event_operators(end)
        }
        flat::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            expression_contains_clocked_or_event_operators(expr)
                || indices
                    .iter()
                    .any(|idx| expression_contains_clocked_or_event_operators(&idx.range))
                || filter
                    .as_deref()
                    .is_some_and(expression_contains_clocked_or_event_operators)
        }
        flat::Expression::Index { base, subscripts } => {
            expression_contains_clocked_or_event_operators(base)
                || subscripts.iter().any(|sub| match sub {
                    flat::Subscript::Expr(expr) => {
                        expression_contains_clocked_or_event_operators(expr)
                    }
                    flat::Subscript::Index(_) | flat::Subscript::Colon => false,
                })
        }
        flat::Expression::FieldAccess { base, .. } => {
            expression_contains_clocked_or_event_operators(base)
        }
        flat::Expression::VarRef { .. }
        | flat::Expression::Literal(_)
        | flat::Expression::Empty => false,
    }
}

fn is_clock_intrinsic_short_name(short_name: &str) -> bool {
    matches!(
        short_name,
        "previous"
            | "Clock"
            | "hold"
            | "subSample"
            | "superSample"
            | "shiftSample"
            | "backSample"
            | "noClock"
            | "firstTick"
            | "interval"
    )
}
