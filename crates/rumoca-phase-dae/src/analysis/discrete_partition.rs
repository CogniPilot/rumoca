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
        match discrete_bucket_for_name(dae, residual, &target) {
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
    residual: &rumoca_core::Expression,
    name: &rumoca_core::VarName,
) -> Option<NameDiscreteBucket> {
    if partition_contains_target_or_scalarized_lane(&dae.variables.discrete_valued, name)
        || residual_target_component_reference(residual, name).is_some_and(|reference| {
            !scalarized_discrete_targets_for_reference(&dae.variables.discrete_valued, &reference)
                .is_empty()
        })
    {
        return Some(NameDiscreteBucket::DiscreteValued);
    }
    if partition_contains_target_or_scalarized_lane(&dae.variables.discrete_reals, name)
        || residual_target_component_reference(residual, name).is_some_and(|reference| {
            !scalarized_discrete_targets_for_reference(&dae.variables.discrete_reals, &reference)
                .is_empty()
        })
    {
        return Some(NameDiscreteBucket::DiscreteReal);
    }
    None
}

fn partition_contains_target_or_scalarized_lane(
    partition: &indexmap::IndexMap<rumoca_core::VarName, dae::Variable>,
    target: &rumoca_core::VarName,
) -> bool {
    if partition.contains_key(&flat_to_dae_var_name(target))
        || subscript_fallback_chain(target.as_str())
            .into_iter()
            .any(|candidate| partition.contains_key(&flat_to_dae_var_name(&candidate)))
    {
        return true;
    }

    false
}

/// Return concrete scalar DAE variables produced from an aggregate equation
/// target. Rendered names are protocol/display data; lane recovery uses the
/// preserved component-reference parts and subscripts.
pub(crate) fn scalarized_discrete_targets_for_reference(
    partition: &indexmap::IndexMap<rumoca_core::VarName, dae::Variable>,
    target_ref: &rumoca_core::ComponentReference,
) -> Vec<rumoca_core::VarName> {
    partition
        .iter()
        .filter_map(|(name, candidate)| {
            let candidate_ref = candidate.component_ref.as_ref()?;
            (candidate.dims.is_empty()
                && component_reference_is_scalar_lane_of(candidate_ref, target_ref))
            .then(|| crate::dae_to_flat_var_name(name))
        })
        .collect()
}

pub(crate) fn residual_target_component_reference(
    residual: &rumoca_core::Expression,
    target: &rumoca_core::VarName,
) -> Option<rumoca_core::ComponentReference> {
    struct Finder<'a> {
        target: &'a rumoca_core::VarName,
        found: Option<rumoca_core::ComponentReference>,
    }

    impl<'a> rumoca_core::ExpressionVisitor for Finder<'a> {
        fn visit_var_ref(
            &mut self,
            name: &rumoca_core::Reference,
            _subscripts: &[rumoca_core::Subscript],
        ) {
            if self.found.is_none() && name.var_name() == self.target {
                self.found = name.component_ref().cloned();
            }
        }
    }

    let mut finder = Finder {
        target,
        found: None,
    };
    finder.visit_expression(residual);
    finder.found
}

fn component_reference_is_scalar_lane_of(
    candidate: &rumoca_core::ComponentReference,
    aggregate: &rumoca_core::ComponentReference,
) -> bool {
    // Instantiation assigns distinct declaration identities to scalar connector
    // elements, while the source aggregate equation retains the unspecialized
    // declaration identity. Full scoped path + structured subscripts therefore
    // define lane identity here; requiring equal DefIds would reject every
    // legitimately scalarized connector array.
    if candidate.parts.len() != aggregate.parts.len() {
        return false;
    }

    let mut added_scalar_subscript = false;
    for (candidate_part, aggregate_part) in candidate.parts.iter().zip(&aggregate.parts) {
        if candidate_part.ident != aggregate_part.ident
            || candidate_part.subs.len() < aggregate_part.subs.len()
            || !candidate_part
                .subs
                .iter()
                .zip(&aggregate_part.subs)
                .all(|(candidate_sub, aggregate_sub)| candidate_sub == aggregate_sub)
        {
            return false;
        }
        if candidate_part.subs.len() > aggregate_part.subs.len() {
            added_scalar_subscript = true;
        }
    }
    added_scalar_subscript
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

#[cfg(test)]
mod tests {
    use super::*;

    fn test_span() -> rumoca_core::Span {
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2)
    }

    fn var_ref(name: &str) -> rumoca_core::Expression {
        let name = rumoca_core::VarName::new(name);
        rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::with_component_reference(
                name.as_str(),
                rumoca_core::component_reference_from_flat_name(&name, test_span()).unwrap(),
            ),
            subscripts: Vec::new(),
            span: test_span(),
        }
    }

    fn residual(lhs: &str, rhs: &str) -> rumoca_core::Expression {
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(var_ref(lhs)),
            rhs: Box::new(var_ref(rhs)),
            span: test_span(),
        }
    }

    fn insert_variable(
        variables: &mut indexmap::IndexMap<rumoca_core::VarName, dae::Variable>,
        name: &str,
    ) {
        let name = rumoca_core::VarName::new(name);
        let mut variable = dae::Variable::new(name.clone(), test_span());
        variable.component_ref =
            rumoca_core::component_reference_from_flat_name(&name, test_span());
        variables.insert(name, variable);
    }

    #[test]
    fn scalarized_boolean_array_target_is_discrete_valued() {
        let mut dae = dae::Dae::new();
        for name in ["parallel.split[1].set", "parallel.split[2].set", "trigger"] {
            insert_variable(&mut dae.variables.discrete_valued, name);
        }
        assert_eq!(
            classify_residual_discrete_bucket(&dae, &residual("parallel.split.set", "trigger")),
            Some(ResidualDiscreteBucket::DiscreteValued),
            "the aggregate Boolean target must inherit the partition of both scalarized lanes"
        );
    }

    #[test]
    fn scalarized_continuous_real_array_target_stays_continuous() {
        let mut dae = dae::Dae::new();
        for name in ["plant.y[1]", "plant.y[2]"] {
            insert_variable(&mut dae.variables.algebraics, name);
        }
        assert_eq!(
            classify_residual_discrete_bucket(&dae, &residual("plant.y", "plant.u")),
            None,
            "continuous Real array equations must remain in f_x"
        );
    }

    #[test]
    fn scalar_lane_identity_does_not_require_unspecialized_def_id() {
        let mut aggregate = rumoca_core::component_reference_from_flat_name(
            &rumoca_core::VarName::new("parallel.split.set"),
            test_span(),
        )
        .unwrap();
        aggregate.def_id = Some(rumoca_core::DefId::new(10));
        let mut lane = rumoca_core::component_reference_from_flat_name(
            &rumoca_core::VarName::new("parallel.split[2].set"),
            test_span(),
        )
        .unwrap();
        lane.def_id = Some(rumoca_core::DefId::new(20));

        assert!(component_reference_is_scalar_lane_of(&lane, &aggregate));
    }
}
