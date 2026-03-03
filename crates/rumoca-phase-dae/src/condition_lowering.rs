//! Canonical DAE condition lowering (MLS Appendix B, B.1d).
//!
//! Builds `relation` + `f_c` from model conditions so solver backends can rely
//! on one canonical root-condition surface.

use rumoca_core::Span;
use rumoca_ir_dae as dae;
use rumoca_ir_flat as flat;
use std::collections::HashSet;

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
            &eq.residual,
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
        if !cond_suppressed {
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
