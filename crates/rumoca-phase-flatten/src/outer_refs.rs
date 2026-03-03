//! Helpers for redirecting outer-prefixed references to resolved inner prefixes (MLS §5.4).

use indexmap::IndexMap;
use rumoca_ir_flat as flat;

/// Redirect outer-prefixed VarRef names in flat model structures (MLS §5.4).
///
/// When outer components are not instantiated, equations from the parent class
/// may reference the outer component's path (e.g., "initialStep.stateGraphRoot.suspend").
/// These must be redirected to the inner instance's path (e.g., "stateGraphRoot.suspend").
pub(crate) fn redirect_outer_refs(
    flat: &mut flat::Model,
    outer_to_inner: &IndexMap<String, String>,
) {
    if outer_to_inner.is_empty() {
        return;
    }
    // Redirect VarRef names in equations.
    for eq in &mut flat.equations {
        redirect_flat_expr(&mut eq.residual, outer_to_inner);
    }
    for eq in &mut flat.initial_equations {
        redirect_flat_expr(&mut eq.residual, outer_to_inner);
    }
    // Redirect when clause conditions and equation targets/values.
    for wc in &mut flat.when_clauses {
        redirect_flat_expr(&mut wc.condition, outer_to_inner);
        redirect_when_equations(&mut wc.equations, outer_to_inner);
    }
    // Redirect algorithm output names.
    for algo in &mut flat.algorithms {
        for out in &mut algo.outputs {
            redirect_var_name(out, outer_to_inner);
        }
    }
    for algo in &mut flat.initial_algorithms {
        for out in &mut algo.outputs {
            redirect_var_name(out, outer_to_inner);
        }
    }
    // Redirect definite_roots.
    let redirected_roots: std::collections::HashSet<String> = flat
        .definite_roots
        .iter()
        .map(|r| redirect_name_string(r, outer_to_inner).unwrap_or_else(|| r.clone()))
        .collect();
    flat.definite_roots = redirected_roots;
}

/// Redirect a flat::VarName if it starts with an outer prefix.
fn redirect_var_name(name: &mut flat::VarName, outer_to_inner: &IndexMap<String, String>) {
    if let Some(new_name) = redirect_name_string(name.as_str(), outer_to_inner) {
        *name = flat::VarName::new(new_name);
    }
}

/// Check if a name starts with an outer prefix and return the redirected version.
fn redirect_name_string(name: &str, outer_to_inner: &IndexMap<String, String>) -> Option<String> {
    for (outer_prefix, inner_prefix) in outer_to_inner {
        if name == outer_prefix.as_str() {
            return Some(inner_prefix.clone());
        }
        let dotted = format!("{outer_prefix}.");
        if name.starts_with(&dotted) {
            return Some(format!("{inner_prefix}{}", &name[outer_prefix.len()..]));
        }
    }
    None
}

/// Recursively redirect outer-prefixed VarRef names in a flat::Expression.
fn redirect_flat_expr(expr: &mut flat::Expression, outer_to_inner: &IndexMap<String, String>) {
    match expr {
        flat::Expression::VarRef { name, subscripts } => {
            redirect_var_name(name, outer_to_inner);
            for sub in subscripts {
                if let flat::Subscript::Expr(e) = sub {
                    redirect_flat_expr(e, outer_to_inner);
                }
            }
        }
        flat::Expression::Binary { lhs, rhs, .. } => {
            redirect_flat_expr(lhs, outer_to_inner);
            redirect_flat_expr(rhs, outer_to_inner);
        }
        flat::Expression::Unary { rhs, .. } => {
            redirect_flat_expr(rhs, outer_to_inner);
        }
        flat::Expression::BuiltinCall { args, .. }
        | flat::Expression::FunctionCall { args, .. } => {
            for arg in args {
                redirect_flat_expr(arg, outer_to_inner);
            }
        }
        flat::Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, body) in branches {
                redirect_flat_expr(cond, outer_to_inner);
                redirect_flat_expr(body, outer_to_inner);
            }
            redirect_flat_expr(else_branch, outer_to_inner);
        }
        flat::Expression::Array { elements, .. } | flat::Expression::Tuple { elements } => {
            for elem in elements {
                redirect_flat_expr(elem, outer_to_inner);
            }
        }
        flat::Expression::Range { start, step, end } => {
            redirect_flat_expr(start, outer_to_inner);
            if let Some(s) = step {
                redirect_flat_expr(s, outer_to_inner);
            }
            redirect_flat_expr(end, outer_to_inner);
        }
        flat::Expression::Index {
            base, subscripts, ..
        } => {
            redirect_flat_expr(base, outer_to_inner);
            for sub in subscripts {
                if let flat::Subscript::Expr(e) = sub {
                    redirect_flat_expr(e, outer_to_inner);
                }
            }
        }
        flat::Expression::FieldAccess { base, .. } => {
            redirect_flat_expr(base, outer_to_inner);
        }
        flat::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            for index in indices {
                redirect_flat_expr(&mut index.range, outer_to_inner);
            }
            redirect_flat_expr(expr, outer_to_inner);
            if let Some(filter_expr) = filter.as_mut() {
                redirect_flat_expr(filter_expr, outer_to_inner);
            }
        }
        flat::Expression::Literal(_) | flat::Expression::Empty => {}
    }
}

/// Redirect outer-prefixed VarRef names in when equations.
fn redirect_when_equations(
    equations: &mut [flat::WhenEquation],
    outer_to_inner: &IndexMap<String, String>,
) {
    for weq in equations.iter_mut() {
        match weq {
            flat::WhenEquation::Assign { target, value, .. } => {
                redirect_var_name(target, outer_to_inner);
                redirect_flat_expr(value, outer_to_inner);
            }
            flat::WhenEquation::Reinit { state, value, .. } => {
                redirect_var_name(state, outer_to_inner);
                redirect_flat_expr(value, outer_to_inner);
            }
            flat::WhenEquation::Assert { condition, .. } => {
                redirect_flat_expr(condition, outer_to_inner);
            }
            flat::WhenEquation::Terminate { .. } => {}
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                for (cond, eqs) in branches {
                    redirect_flat_expr(cond, outer_to_inner);
                    redirect_when_equations(eqs, outer_to_inner);
                }
                redirect_when_equations(else_branch, outer_to_inner);
            }
            flat::WhenEquation::FunctionCallOutputs {
                outputs, function, ..
            } => {
                for out in outputs {
                    redirect_var_name(out, outer_to_inner);
                }
                redirect_flat_expr(function, outer_to_inner);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::Span;
    use rumoca_ir_flat::{
        Algorithm, ComprehensionIndex, Equation, EquationOrigin, Literal, WhenClause,
    };

    fn var_ref(name: &str) -> flat::Expression {
        flat::Expression::VarRef {
            name: flat::VarName::new(name),
            subscripts: Vec::new(),
        }
    }

    #[test]
    fn test_redirect_name_string_handles_exact_and_prefixed_matches() {
        let mut outer_to_inner = IndexMap::new();
        outer_to_inner.insert("outerBus".to_string(), "innerBus".to_string());

        assert_eq!(
            redirect_name_string("outerBus", &outer_to_inner),
            Some("innerBus".to_string())
        );
        assert_eq!(
            redirect_name_string("outerBus.signal", &outer_to_inner),
            Some("innerBus.signal".to_string())
        );
        assert_eq!(redirect_name_string("other.signal", &outer_to_inner), None);
    }

    #[test]
    fn test_redirect_outer_refs_updates_equations_whens_algorithms_and_roots() {
        let mut flat = flat::Model::new();
        flat.equations.push(Equation::new(
            var_ref("outerBus.signal"),
            Span::DUMMY,
            EquationOrigin::Binding {
                variable: "x".to_string(),
            },
        ));
        flat.initial_equations.push(Equation::new(
            flat::Expression::Range {
                start: Box::new(var_ref("outerBus.start")),
                step: None,
                end: Box::new(flat::Expression::Literal(Literal::Integer(1))),
            },
            Span::DUMMY,
            EquationOrigin::Binding {
                variable: "x0".to_string(),
            },
        ));

        let mut when_clause = WhenClause::new(var_ref("outerBus.trigger"), Span::DUMMY);
        when_clause.add_equation(flat::WhenEquation::Assign {
            target: flat::VarName::new("outerBus.target"),
            value: var_ref("outerBus.value"),
            span: Span::DUMMY,
            origin: "test".to_string(),
        });
        flat.when_clauses.push(when_clause);

        flat.algorithms.push(Algorithm {
            statements: Vec::new(),
            outputs: vec![flat::VarName::new("outerBus.algOut")],
            span: Span::DUMMY,
            origin: "test".to_string(),
        });
        flat.initial_algorithms.push(Algorithm {
            statements: Vec::new(),
            outputs: vec![flat::VarName::new("outerBus.initOut")],
            span: Span::DUMMY,
            origin: "test".to_string(),
        });
        flat.definite_roots.insert("outerBus.root".to_string());

        let mut outer_to_inner = IndexMap::new();
        outer_to_inner.insert("outerBus".to_string(), "innerBus".to_string());
        redirect_outer_refs(&mut flat, &outer_to_inner);

        let flat::Expression::VarRef { name, .. } = &flat.equations[0].residual else {
            panic!("expected var ref");
        };
        assert_eq!(name.as_str(), "innerBus.signal");

        let flat::Expression::Range { start, .. } = &flat.initial_equations[0].residual else {
            panic!("expected range expression");
        };
        let flat::Expression::VarRef { name, .. } = start.as_ref() else {
            panic!("expected range start var ref");
        };
        assert_eq!(name.as_str(), "innerBus.start");

        let flat::Expression::VarRef { name, .. } = &flat.when_clauses[0].condition else {
            panic!("expected when condition var ref");
        };
        assert_eq!(name.as_str(), "innerBus.trigger");
        let flat::WhenEquation::Assign { target, value, .. } = &flat.when_clauses[0].equations[0]
        else {
            panic!("expected assign equation");
        };
        assert_eq!(target.as_str(), "innerBus.target");
        let flat::Expression::VarRef { name, .. } = value else {
            panic!("expected assign value var ref");
        };
        assert_eq!(name.as_str(), "innerBus.value");

        assert_eq!(flat.algorithms[0].outputs[0].as_str(), "innerBus.algOut");
        assert_eq!(
            flat.initial_algorithms[0].outputs[0].as_str(),
            "innerBus.initOut"
        );
        assert!(flat.definite_roots.contains("innerBus.root"));
        assert!(!flat.definite_roots.contains("outerBus.root"));
    }

    #[test]
    fn test_redirect_outer_refs_updates_array_comprehension_members() {
        let mut expr = flat::Expression::ArrayComprehension {
            expr: Box::new(var_ref("outerBus.value")),
            indices: vec![ComprehensionIndex {
                name: "i".to_string(),
                range: var_ref("outerBus.range"),
            }],
            filter: Some(Box::new(var_ref("outerBus.filter"))),
        };

        let mut outer_to_inner = IndexMap::new();
        outer_to_inner.insert("outerBus".to_string(), "innerBus".to_string());
        redirect_flat_expr(&mut expr, &outer_to_inner);

        let flat::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } = expr
        else {
            panic!("expected array comprehension");
        };

        let flat::Expression::VarRef { name, .. } = expr.as_ref() else {
            panic!("expected comprehension body var ref");
        };
        assert_eq!(name.as_str(), "innerBus.value");

        let flat::Expression::VarRef { name, .. } = &indices[0].range else {
            panic!("expected comprehension range var ref");
        };
        assert_eq!(name.as_str(), "innerBus.range");

        let Some(filter_expr) = filter else {
            panic!("expected comprehension filter");
        };
        let flat::Expression::VarRef { name, .. } = filter_expr.as_ref() else {
            panic!("expected comprehension filter var ref");
        };
        assert_eq!(name.as_str(), "innerBus.filter");
    }
}
