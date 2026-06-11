//! Helpers for partitioning clocked/discrete equations in ToDAE.
//!
//! MLS Appendix B separates continuous equations (`f_x`) from discrete updates
//! (`f_z`/`f_m`). This module provides shared classification helpers so variable
//! and equation partitioning use the same rules.

use std::collections::HashSet;

use rumoca_core::ExpressionVisitor;
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
/// contains clocked operators (`previous`, `sample`, ...).
pub(crate) fn is_clocked_assignment_equation(eq: &flat::Equation) -> bool {
    !residual_lhs_targets(&eq.residual).is_empty()
        && expression_contains_clocked_operators(&eq.residual)
}

/// Collect assignment targets from residual form `lhs - rhs = 0`.
///
/// Supports tuple/array LHS by collecting all variable references in the LHS.
pub(crate) fn residual_lhs_targets(
    residual: &rumoca_core::Expression,
) -> Vec<rumoca_core::VarName> {
    let mut targets = Vec::new();
    match residual {
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs,
            rhs,
            ..
        } => {
            collect_lhs_targets(lhs, &mut targets);
            extend_wrapped_assignment_targets(lhs, rhs, &mut targets);
        }
        // Some flattened if-equations are already in residual form without an
        // outer `0 - (...)` wrapper. Recover assignment targets from active
        // branch residuals so discrete partitioning remains spec-aligned.
        rumoca_core::Expression::If { .. }
        | rumoca_core::Expression::Unary {
            op: rumoca_core::OpUnary::Minus,
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
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    out: &mut Vec<rumoca_core::VarName>,
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

fn extend_assignment_targets(expr: &rumoca_core::Expression, out: &mut Vec<rumoca_core::VarName>) {
    let mut candidates = Vec::new();
    if collect_assignment_targets_from_residual_rhs(expr, &mut candidates) {
        out.extend(candidates);
    }
}

fn is_numeric_zero(expr: &rumoca_core::Expression) -> bool {
    match expr {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(0),
            ..
        } => true,
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(v),
            ..
        } => v.abs() <= f64::EPSILON,
        _ => false,
    }
}

fn collect_assignment_targets_from_residual_rhs(
    expr: &rumoca_core::Expression,
    out: &mut Vec<rumoca_core::VarName>,
) -> bool {
    match expr {
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs,
            ..
        } => {
            let before = out.len();
            collect_lhs_targets(lhs, out);
            out.len() > before
        }
        rumoca_core::Expression::Unary {
            op: rumoca_core::OpUnary::Minus,
            rhs,
            ..
        } => collect_assignment_targets_from_residual_rhs(rhs, out),
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
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
        rumoca_core::Expression::Tuple { elements, .. }
        | rumoca_core::Expression::Array { elements, .. } => {
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
    residual: &rumoca_core::Expression,
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

fn collect_lhs_targets(lhs: &rumoca_core::Expression, out: &mut Vec<rumoca_core::VarName>) {
    match lhs {
        rumoca_core::Expression::VarRef { name, .. } => out.push(name.var_name().clone()),
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Der,
            args,
            ..
        } if args.len() == 1 => collect_lhs_targets(&args[0], out),
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (_, value) in branches {
                collect_lhs_targets(value, out);
            }
            collect_lhs_targets(else_branch, out);
        }
        rumoca_core::Expression::Unary { rhs, .. } => collect_lhs_targets(rhs, out),
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            collect_lhs_targets(base, out);
            for sub in subscripts {
                if let rumoca_core::Subscript::Expr { expr, .. } = sub {
                    collect_lhs_targets(expr, out);
                }
            }
        }
        rumoca_core::Expression::FieldAccess { base, .. } => collect_lhs_targets(base, out),
        rumoca_core::Expression::Tuple { elements, .. }
        | rumoca_core::Expression::Array { elements, .. } => {
            for element in elements {
                collect_lhs_targets(element, out);
            }
        }
        _ => {}
    }
}

fn discrete_bucket_for_name(
    dae: &dae::Dae,
    name: &rumoca_core::VarName,
) -> Option<NameDiscreteBucket> {
    if dae
        .variables
        .discrete_valued
        .contains_key(&flat_to_dae_var_name(name))
        || subscript_fallback_chain(name.as_str())
            .into_iter()
            .any(|candidate| {
                dae.variables
                    .discrete_valued
                    .contains_key(&flat_to_dae_var_name(&candidate))
            })
    {
        return Some(NameDiscreteBucket::DiscreteValued);
    }
    if dae
        .variables
        .discrete_reals
        .contains_key(&flat_to_dae_var_name(name))
        || subscript_fallback_chain(name.as_str())
            .into_iter()
            .any(|candidate| {
                dae.variables
                    .discrete_reals
                    .contains_key(&flat_to_dae_var_name(&candidate))
            })
    {
        return Some(NameDiscreteBucket::DiscreteReal);
    }
    None
}

/// Returns true when expression contains clocked primitives that make the
/// assignment target a clocked/discrete-time value.
///
/// `pre(x)` is intentionally excluded: a continuous equation may depend on the
/// previous value of a declared discrete variable without making its own LHS a
/// discrete assignment.
pub(crate) fn expression_contains_clocked_operators(expr: &rumoca_core::Expression) -> bool {
    let mut checker = ClockedOperatorChecker { found: false };
    checker.visit_expression(expr);
    checker.found
}

struct ClockedOperatorChecker {
    found: bool,
}

impl ExpressionVisitor for ClockedOperatorChecker {
    fn visit_expression(&mut self, expr: &rumoca_core::Expression) {
        if !self.found {
            self.walk_expression(expr);
        }
    }

    fn visit_builtin_call(
        &mut self,
        function: &rumoca_core::BuiltinFunction,
        args: &[rumoca_core::Expression],
    ) {
        if matches!(
            function,
            rumoca_core::BuiltinFunction::Sample
                | rumoca_core::BuiltinFunction::Edge
                | rumoca_core::BuiltinFunction::Change
        ) {
            self.found = true;
            return;
        }
        self.walk_builtin_call(function, args);
    }

    fn visit_function_call(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        is_constructor: bool,
    ) {
        let short_name = name.last_segment();
        if is_clock_intrinsic_short_name(short_name) {
            self.found = true;
            return;
        }
        self.walk_function_call(name, args, is_constructor);
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
