//! Canonical DAE condition lowering (MLS Appendix B, B.1d).
//!
//! Builds `relation` + `f_c` from model conditions so solver backends can rely
//! on one canonical root-condition surface.

use rumoca_core::Span;
use rumoca_ir_dae as dae;
use rumoca_ir_flat as flat;
use std::collections::HashSet;

use crate::flat_to_dae_expression;

#[derive(Debug, Clone)]
struct ConditionCandidate {
    expr: dae::Expression,
    span: Span,
    source: String,
}

pub(crate) fn populate_canonical_conditions(dae_model: &mut dae::Dae, flat_model: &flat::Model) {
    let mut candidates = Vec::new();

    for eq in &flat_model.equations {
        collect_if_condition_candidates(
            &flat_to_dae_expression(&eq.residual),
            eq.span,
            format!("{}", eq.origin),
            false,
            &mut candidates,
        );
    }

    for eq in &dae_model.f_x {
        collect_if_condition_candidates(
            &eq.rhs,
            eq.span,
            eq.origin.clone(),
            false,
            &mut candidates,
        );
    }

    for eq in dae_model.f_z.iter().chain(dae_model.f_m.iter()) {
        collect_if_condition_candidates(
            &eq.rhs,
            eq.span,
            eq.origin.clone(),
            false,
            &mut candidates,
        );
    }

    dedupe_condition_candidates(&mut candidates);

    dae_model.relation = candidates
        .iter()
        .map(|candidate| candidate.expr.clone())
        .collect();
    dae_model.f_c = candidates
        .iter()
        .enumerate()
        .map(|(idx, candidate)| build_condition_equation(candidate, idx + 1))
        .collect();
}

fn build_condition_equation(
    candidate: &ConditionCandidate,
    condition_index: usize,
) -> dae::Equation {
    dae::Equation::explicit(
        dae::VarName::new(format!("c[{condition_index}]")),
        candidate.expr.clone(),
        candidate.span,
        format!("condition equation from {}", candidate.source),
    )
}

fn dedupe_condition_candidates(candidates: &mut Vec<ConditionCandidate>) {
    let mut seen = HashSet::new();
    candidates.retain(|candidate| seen.insert(format!("{:?}", candidate.expr)));
}

fn is_event_suppressed_wrapper(expr: &dae::Expression) -> bool {
    matches!(
        expr,
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::NoEvent | dae::BuiltinFunction::Smooth,
            ..
        }
    )
}

fn is_relation_extracting_event_wrapper(expr: &dae::Expression) -> bool {
    matches!(
        expr,
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Edge | dae::BuiltinFunction::Change,
            ..
        }
    )
}

fn is_non_relation_condition(expr: &dae::Expression) -> bool {
    matches!(
        expr,
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Initial,
            ..
        }
    )
}

fn is_relational_binary_op(op: &rumoca_ir_core::OpBinary) -> bool {
    matches!(
        op,
        rumoca_ir_core::OpBinary::Lt(_)
            | rumoca_ir_core::OpBinary::Le(_)
            | rumoca_ir_core::OpBinary::Gt(_)
            | rumoca_ir_core::OpBinary::Ge(_)
            | rumoca_ir_core::OpBinary::Eq(_)
            | rumoca_ir_core::OpBinary::Neq(_)
    )
}

fn expression_contains_relational_operator(expr: &dae::Expression) -> bool {
    match expr {
        dae::Expression::Binary { op, lhs, rhs } => {
            is_relational_binary_op(op)
                || expression_contains_relational_operator(lhs)
                || expression_contains_relational_operator(rhs)
        }
        dae::Expression::Unary { rhs, .. } => expression_contains_relational_operator(rhs),
        dae::Expression::BuiltinCall { args, .. } | dae::Expression::FunctionCall { args, .. } => {
            args.iter().any(expression_contains_relational_operator)
        }
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(cond, value)| {
                expression_contains_relational_operator(cond)
                    || expression_contains_relational_operator(value)
            }) || expression_contains_relational_operator(else_branch)
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            elements.iter().any(expression_contains_relational_operator)
        }
        dae::Expression::Range { start, step, end } => {
            expression_contains_relational_operator(start)
                || step
                    .as_deref()
                    .is_some_and(expression_contains_relational_operator)
                || expression_contains_relational_operator(end)
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            expression_contains_relational_operator(expr)
                || indices
                    .iter()
                    .any(|idx| expression_contains_relational_operator(&idx.range))
                || filter
                    .as_deref()
                    .is_some_and(expression_contains_relational_operator)
        }
        dae::Expression::Index { base, subscripts } => {
            expression_contains_relational_operator(base)
                || subscripts.iter().any(|sub| match sub {
                    dae::Subscript::Expr(expr) => expression_contains_relational_operator(expr),
                    dae::Subscript::Index(_) | dae::Subscript::Colon => false,
                })
        }
        dae::Expression::FieldAccess { base, .. } => expression_contains_relational_operator(base),
        dae::Expression::VarRef { .. } | dae::Expression::Literal(_) | dae::Expression::Empty => {
            false
        }
    }
}

fn collect_if_condition_candidates(
    expr: &dae::Expression,
    span: Span,
    source: String,
    suppress_events: bool,
    out: &mut Vec<ConditionCandidate>,
) {
    match expr {
        dae::Expression::If {
            branches,
            else_branch,
        } => collect_if_condition_candidates_for_if(
            branches,
            else_branch,
            span,
            source,
            suppress_events,
            out,
        ),
        dae::Expression::Binary { lhs, rhs, .. } => {
            collect_if_condition_candidates(lhs, span, source.clone(), suppress_events, out);
            collect_if_condition_candidates(rhs, span, source, suppress_events, out);
        }
        dae::Expression::Unary { rhs, .. } => {
            collect_if_condition_candidates(rhs, span, source, suppress_events, out);
        }
        dae::Expression::BuiltinCall { function, args } => {
            collect_if_condition_candidates_for_builtin(
                *function,
                args,
                span,
                source,
                suppress_events,
                out,
            );
        }
        dae::Expression::FunctionCall { args, .. } => {
            for arg in args {
                collect_if_condition_candidates(arg, span, source.clone(), suppress_events, out);
            }
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            for element in elements {
                collect_if_condition_candidates(
                    element,
                    span,
                    source.clone(),
                    suppress_events,
                    out,
                );
            }
        }
        dae::Expression::Range { start, step, end } => {
            collect_if_condition_candidates(start, span, source.clone(), suppress_events, out);
            if let Some(step_expr) = step {
                collect_if_condition_candidates(
                    step_expr,
                    span,
                    source.clone(),
                    suppress_events,
                    out,
                );
            }
            collect_if_condition_candidates(end, span, source, suppress_events, out);
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => collect_if_condition_candidates_for_array_comprehension(
            expr,
            indices,
            filter.as_deref(),
            span,
            source,
            suppress_events,
            out,
        ),
        dae::Expression::Index { base, subscripts } => {
            collect_if_condition_candidates_for_index(
                base,
                subscripts,
                span,
                source,
                suppress_events,
                out,
            );
        }
        dae::Expression::FieldAccess { base, .. } => {
            collect_if_condition_candidates(base, span, source, suppress_events, out);
        }
        dae::Expression::VarRef { .. } | dae::Expression::Literal(_) | dae::Expression::Empty => {}
    }
}

fn collect_if_condition_candidates_for_if(
    branches: &[(dae::Expression, dae::Expression)],
    else_branch: &dae::Expression,
    span: Span,
    source: String,
    suppress_events: bool,
    out: &mut Vec<ConditionCandidate>,
) {
    for (condition, value) in branches {
        let cond_suppressed = suppress_events || is_event_suppressed_wrapper(condition);
        // MLS Appendix B B.1d: canonical conditions live on relation(v), so
        // event combinators like edge/change contribute their underlying
        // relational guard via the builtin walk below, not as wrapper roots.
        if !cond_suppressed
            && !is_relation_extracting_event_wrapper(condition)
            && !is_non_relation_condition(condition)
        {
            out.push(ConditionCandidate {
                expr: condition.clone(),
                span,
                source: source.clone(),
            });
        }
        collect_if_condition_candidates(condition, span, source.clone(), cond_suppressed, out);
        collect_if_condition_candidates(value, span, source.clone(), suppress_events, out);
    }
    collect_if_condition_candidates(else_branch, span, source, suppress_events, out);
}

fn collect_if_condition_candidates_for_builtin(
    function: dae::BuiltinFunction,
    args: &[dae::Expression],
    span: Span,
    source: String,
    suppress_events: bool,
    out: &mut Vec<ConditionCandidate>,
) {
    let suppressed = suppress_events
        || matches!(
            function,
            dae::BuiltinFunction::NoEvent | dae::BuiltinFunction::Smooth
        );
    if !suppressed
        && matches!(
            function,
            dae::BuiltinFunction::Edge | dae::BuiltinFunction::Change
        )
        && let Some(arg) = args.first()
        && expression_contains_relational_operator(arg)
    {
        out.push(ConditionCandidate {
            expr: arg.clone(),
            span,
            source: source.clone(),
        });
    }
    for arg in args {
        collect_if_condition_candidates(arg, span, source.clone(), suppressed, out);
    }
}

fn collect_if_condition_candidates_for_array_comprehension(
    expr: &dae::Expression,
    indices: &[dae::ComprehensionIndex],
    filter: Option<&dae::Expression>,
    span: Span,
    source: String,
    suppress_events: bool,
    out: &mut Vec<ConditionCandidate>,
) {
    collect_if_condition_candidates(expr, span, source.clone(), suppress_events, out);
    for index in indices {
        collect_if_condition_candidates(&index.range, span, source.clone(), suppress_events, out);
    }
    if let Some(filter_expr) = filter {
        collect_if_condition_candidates(filter_expr, span, source, suppress_events, out);
    }
}

fn collect_if_condition_candidates_for_index(
    base: &dae::Expression,
    subscripts: &[dae::Subscript],
    span: Span,
    source: String,
    suppress_events: bool,
    out: &mut Vec<ConditionCandidate>,
) {
    collect_if_condition_candidates(base, span, source.clone(), suppress_events, out);
    for subscript in subscripts {
        if let dae::Subscript::Expr(sub_expr) = subscript {
            collect_if_condition_candidates(sub_expr, span, source.clone(), suppress_events, out);
        }
    }
}
