//! Runtime-defined unknown name resolution.
//!
//! These functions identify which DAE unknowns are assigned at event/clock
//! runtime (f_z, f_m equations, or f_x assignments using pre()/sample()/etc.)
//! rather than solved by the continuous solver.
//!
//! AGENTS.md: "`runtime_defined_unknown_names` lives in
//! `rumoca-phase-structural::runtime_defined`."

use std::collections::HashSet;

use indexmap::IndexSet;
use rumoca_core::ExpressionVisitor;
use rumoca_ir_dae as dae;

/// Names of unknowns defined at runtime by event/clock evaluation.
///
/// Includes direct targets and expanded record fields.
/// Returns an `IndexSet` to guarantee deterministic insertion-order iteration.
pub fn runtime_defined_unknown_names(dae_model: &dae::Dae) -> IndexSet<String> {
    runtime_defined_impl(dae_model, true)
}

/// Names of continuous unknowns defined at runtime by event/clock evaluation.
///
/// Includes direct targets and expanded record fields in `algebraics`/`outputs`.
/// Returns an `IndexSet` to guarantee deterministic insertion-order iteration.
pub fn runtime_defined_continuous_unknown_names(dae_model: &dae::Dae) -> IndexSet<String> {
    runtime_defined_impl(dae_model, false)
}

fn runtime_defined_impl(dae_model: &dae::Dae, include_discrete: bool) -> IndexSet<String> {
    let mut defined = IndexSet::new();

    for eq in dae_model
        .discrete
        .real_updates
        .iter()
        .chain(dae_model.discrete.valued_updates.iter())
    {
        if let Some(lhs) = eq.lhs.as_ref() {
            extend_target(dae_model, &mut defined, lhs.var_name(), include_discrete);
        }
        for target in binary_var_refs(&eq.rhs) {
            extend_target(dae_model, &mut defined, target, include_discrete);
        }
    }

    for expr in dae_model
        .discrete
        .real_updates
        .iter()
        .map(|eq| &eq.rhs)
        .chain(dae_model.discrete.valued_updates.iter().map(|eq| &eq.rhs))
        .chain(dae_model.conditions.equations.iter().map(|eq| &eq.rhs))
        .chain(dae_model.conditions.relations.iter())
    {
        extend_refs_from_expr(dae_model, &mut defined, expr, include_discrete);
    }

    for eq in &dae_model.continuous.equations {
        let Some(target) = binary_var_refs(&eq.rhs).into_iter().next() else {
            continue;
        };
        let Some(solution) = binary_solution_expr(&eq.rhs) else {
            continue;
        };
        if contains_clocked_or_event(solution) {
            extend_target(dae_model, &mut defined, target, include_discrete);
        }
    }

    defined
}

fn extend_refs_from_expr(
    dae_model: &dae::Dae,
    defined: &mut IndexSet<String>,
    expr: &rumoca_core::Expression,
    include_discrete: bool,
) {
    let mut refs = HashSet::new();
    expr.collect_var_refs(&mut refs);
    let mut refs: Vec<_> = refs.into_iter().collect();
    refs.sort_by(|a, b| a.as_str().cmp(b.as_str()));
    for name in refs {
        extend_target(dae_model, defined, &name, include_discrete);
    }
}

fn extend_target(
    dae_model: &dae::Dae,
    defined: &mut IndexSet<String>,
    target: &rumoca_core::VarName,
    include_discrete: bool,
) {
    use std::collections::VecDeque;

    if !include_discrete
        && (dae_model.variables.discrete_reals.contains_key(target)
            || dae_model.variables.discrete_valued.contains_key(target))
    {
        return;
    }

    let raw_target = target.as_str();
    let mut candidates = VecDeque::from([raw_target.to_string()]);
    if let Some(base) = dae::component_base_name(raw_target)
        && base != raw_target
    {
        candidates.push_back(base);
    }

    while let Some(candidate) = candidates.pop_front() {
        let prefix = format!("{candidate}.");
        if include_discrete {
            insert_matching(
                defined,
                &candidate,
                &prefix,
                dae_model
                    .variables
                    .states
                    .keys()
                    .chain(dae_model.variables.algebraics.keys())
                    .chain(dae_model.variables.outputs.keys())
                    .chain(dae_model.variables.discrete_reals.keys())
                    .chain(dae_model.variables.discrete_valued.keys()),
            );
        } else {
            insert_matching(
                defined,
                &candidate,
                &prefix,
                dae_model
                    .variables
                    .algebraics
                    .keys()
                    .chain(dae_model.variables.outputs.keys()),
            );
        }
    }
}

fn insert_matching<'a, I>(defined: &mut IndexSet<String>, candidate: &str, prefix: &str, names: I)
where
    I: Iterator<Item = &'a rumoca_core::VarName>,
{
    for text in names
        .map(rumoca_core::VarName::as_str)
        .filter(|text| *text == candidate || text.starts_with(prefix))
    {
        defined.insert(text.to_string());
    }
}

/// Extract the non-VarRef side of a `lhs - rhs` binary equation (the solution expr).
fn binary_solution_expr(expr: &rumoca_core::Expression) -> Option<&rumoca_core::Expression> {
    let rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs,
        rhs,
        span: _,
    } = expr
    else {
        return None;
    };

    if matches!(lhs.as_ref(), rumoca_core::Expression::VarRef { .. }) {
        return Some(rhs.as_ref());
    }
    if matches!(rhs.as_ref(), rumoca_core::Expression::VarRef { .. }) {
        return Some(lhs.as_ref());
    }
    None
}

/// Extract VarName references from a `lhs - rhs` binary expression.
fn binary_var_refs(expr: &rumoca_core::Expression) -> Vec<&rumoca_core::VarName> {
    let rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs,
        rhs,
        span: _,
    } = expr
    else {
        return Vec::new();
    };

    let mut names = Vec::with_capacity(2);
    if let rumoca_core::Expression::VarRef { name, .. } = lhs.as_ref() {
        names.push(name.var_name());
    }
    if let rumoca_core::Expression::VarRef { name, .. } = rhs.as_ref() {
        names.push(name.var_name());
    }
    names
}

fn contains_clocked_or_event(expr: &rumoca_core::Expression) -> bool {
    let mut checker = ClockedOrEventChecker { found: false };
    checker.visit_expression(expr);
    checker.found
}

struct ClockedOrEventChecker {
    found: bool,
}

pub(crate) fn clocked_or_event_function_short_name(
    name: &rumoca_core::Reference,
) -> Option<&'static str> {
    match name.last_segment() {
        "previous" => Some("previous"),
        "Clock" => Some("Clock"),
        "hold" => Some("hold"),
        "subSample" => Some("subSample"),
        "superSample" => Some("superSample"),
        "shiftSample" => Some("shiftSample"),
        "backSample" => Some("backSample"),
        "noClock" => Some("noClock"),
        "firstTick" => Some("firstTick"),
        "interval" => Some("interval"),
        _ => None,
    }
}

impl ExpressionVisitor for ClockedOrEventChecker {
    fn visit_expression(&mut self, expr: &rumoca_core::Expression) {
        if self.found {
            return;
        }
        match expr {
            rumoca_core::Expression::BuiltinCall {
                function:
                    rumoca_core::BuiltinFunction::Pre
                    | rumoca_core::BuiltinFunction::Sample
                    | rumoca_core::BuiltinFunction::Edge
                    | rumoca_core::BuiltinFunction::Change,
                ..
            } => {
                self.found = true;
            }
            rumoca_core::Expression::FunctionCall { name, .. }
                if clocked_or_event_function_short_name(name).is_some() =>
            {
                self.found = true;
            }
            _ => self.walk_expression(expr),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::Span;

    #[test]
    fn test_runtime_defined_unknown_names_include_discrete_targets() {
        let mut dae = dae::Dae::default();
        dae.variables.algebraics.insert(
            rumoca_core::VarName::new("a"),
            dae::Variable {
                name: rumoca_core::VarName::new("a"),
                ..Default::default()
            },
        );
        dae.variables.discrete_valued.insert(
            rumoca_core::VarName::new("enable"),
            dae::Variable {
                name: rumoca_core::VarName::new("enable"),
                ..Default::default()
            },
        );
        dae.discrete.valued_updates.push(dae::Equation::explicit(
            rumoca_core::VarName::new("a"),
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(1.0),
                span: rumoca_core::Span::DUMMY,
            },
            Span::DUMMY,
            "runtime-defined-a",
        ));
        dae.discrete.valued_updates.push(dae::Equation::explicit(
            rumoca_core::VarName::new("enable"),
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Boolean(true),
                span: rumoca_core::Span::DUMMY,
            },
            Span::DUMMY,
            "runtime-defined-enable",
        ));

        let all = runtime_defined_unknown_names(&dae);
        assert!(all.contains("a"));
        assert!(all.contains("enable"));
    }

    #[test]
    fn test_runtime_defined_continuous_unknown_names_exclude_discrete_targets() {
        let mut dae = dae::Dae::default();
        dae.variables.algebraics.insert(
            rumoca_core::VarName::new("a"),
            dae::Variable {
                name: rumoca_core::VarName::new("a"),
                ..Default::default()
            },
        );
        dae.variables.discrete_valued.insert(
            rumoca_core::VarName::new("enable"),
            dae::Variable {
                name: rumoca_core::VarName::new("enable"),
                ..Default::default()
            },
        );
        dae.discrete.valued_updates.push(dae::Equation::explicit(
            rumoca_core::VarName::new("a"),
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(1.0),
                span: rumoca_core::Span::DUMMY,
            },
            Span::DUMMY,
            "runtime-defined-a",
        ));
        dae.discrete.valued_updates.push(dae::Equation::explicit(
            rumoca_core::VarName::new("enable"),
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Boolean(true),
                span: rumoca_core::Span::DUMMY,
            },
            Span::DUMMY,
            "runtime-defined-enable",
        ));

        let continuous = runtime_defined_continuous_unknown_names(&dae);
        assert!(continuous.contains("a"));
        assert!(!continuous.contains("enable"));
    }

    #[test]
    fn test_runtime_defined_continuous_unknown_names_include_fx_pre_assignment_targets() {
        let mut dae = dae::Dae::default();
        dae.variables.algebraics.insert(
            rumoca_core::VarName::new("gate.y"),
            dae::Variable {
                name: rumoca_core::VarName::new("gate.y"),
                ..Default::default()
            },
        );
        dae.variables.algebraics.insert(
            rumoca_core::VarName::new("gate.aux"),
            dae::Variable {
                name: rumoca_core::VarName::new("gate.aux"),
                ..Default::default()
            },
        );
        dae.continuous.equations.push(dae::Equation::residual(
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(rumoca_core::Expression::VarRef {
                    name: rumoca_core::Reference::new("gate.y"),
                    subscripts: vec![],
                    span: rumoca_core::Span::DUMMY,
                }),
                rhs: Box::new(rumoca_core::Expression::BuiltinCall {
                    function: rumoca_core::BuiltinFunction::Pre,
                    args: vec![rumoca_core::Expression::VarRef {
                        name: rumoca_core::Reference::new("gate.aux"),
                        subscripts: vec![],
                        span: rumoca_core::Span::DUMMY,
                    }],
                    span: rumoca_core::Span::DUMMY,
                }),
                span: rumoca_core::Span::DUMMY,
            },
            Span::DUMMY,
            "equation from gate",
        ));

        let continuous = runtime_defined_continuous_unknown_names(&dae);
        assert!(
            continuous.contains("gate.y"),
            "f_x assignment targets using pre() must remain runtime-defined"
        );
    }
}
