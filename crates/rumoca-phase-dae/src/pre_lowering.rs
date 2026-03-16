//! Lower `pre()` operator references into dedicated parameter symbols.
//!
//! MLS §3.7.5: `pre(y)` returns the left limit of a discrete variable y at an
//! event instant. CasADi and other numeric backends don't have a native `pre()`
//! function. This pass replaces `pre(x)` with a parameter variable `__pre__.x`
//! that the simulation driver updates at each event boundary.

use indexmap::IndexMap;
use rumoca_ir_dae as dae;
use std::collections::HashMap;

/// Lower all `pre()` calls in f_x, f_z, and f_m equations to parameter references.
///
/// For each unique `pre(x)` reference:
/// 1. Creates a parameter `__pre__.x` with the same dimensions and start value as `x`
/// 2. Replaces the `BuiltinCall { Pre, [VarRef(x)] }` with `VarRef("__pre__.x")`
pub(crate) fn lower_pre_operator(dae: &mut dae::Dae) {
    // Collect pre() targets only from continuous equations (f_x).
    // Discrete update equations (f_z, f_m) and conditions (f_c) keep pre()
    // as-is because:
    // - f_z/f_m: pre() is semantically correct for event update rules
    // - f_c: conditions are paired with relation[] and must be preserved
    // IndexMap for deterministic parameter insertion order (SPEC_0017).
    let mut pre_targets: IndexMap<dae::VarName, PreTarget> = IndexMap::new();
    collect_pre_targets_from_equations(&dae.f_x, &mut pre_targets);

    if pre_targets.is_empty() {
        return;
    }

    // Look up variable info for each target to get dims and start values
    let all_vars = collect_all_variables(dae);
    let mut pre_params: IndexMap<dae::VarName, dae::Variable> = IndexMap::new();

    for target_name in pre_targets.keys() {
        let pre_param_name = dae::VarName::new(format!("__pre__.{}", target_name.as_str()));
        let (dims, start) = if let Some(var) = all_vars.get(target_name) {
            (var.dims.clone(), var.start.clone())
        } else {
            (vec![], None)
        };

        let pre_var = dae::Variable {
            name: pre_param_name.clone(),
            dims,
            start,
            fixed: Some(true),
            min: None,
            max: None,
            nominal: None,
            unit: None,
            state_select: rumoca_ir_core::StateSelect::Default,
            description: Some(format!("pre() of {}", target_name.as_str())),
        };
        pre_params.insert(pre_param_name, pre_var);
    }

    // Add pre-parameters to the DAE
    for (name, var) in pre_params {
        dae.parameters.insert(name, var);
    }

    // Rewrite only continuous equations to replace pre() calls.
    rewrite_equations(&mut dae.f_x, &pre_targets);
}

struct PreTarget;

fn collect_pre_targets_from_equations(
    equations: &[dae::Equation],
    targets: &mut IndexMap<dae::VarName, PreTarget>,
) {
    for eq in equations {
        collect_pre_targets_from_expr(&eq.rhs, targets);
    }
}

fn collect_pre_targets_from_expr(
    expr: &dae::Expression,
    targets: &mut IndexMap<dae::VarName, PreTarget>,
) {
    match expr {
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Pre,
            args,
        } => {
            if let Some(dae::Expression::VarRef { name, .. }) = args.first() {
                targets.entry(name.clone()).or_insert(PreTarget);
            }
            // Also recurse into args in case of nested pre() (unlikely but safe)
            for arg in args {
                collect_pre_targets_from_expr(arg, targets);
            }
        }
        dae::Expression::Binary { lhs, rhs, .. } => {
            collect_pre_targets_from_expr(lhs, targets);
            collect_pre_targets_from_expr(rhs, targets);
        }
        dae::Expression::Unary { rhs, .. } => {
            collect_pre_targets_from_expr(rhs, targets);
        }
        dae::Expression::BuiltinCall { args, .. } | dae::Expression::FunctionCall { args, .. } => {
            for arg in args {
                collect_pre_targets_from_expr(arg, targets);
            }
        }
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                collect_pre_targets_from_expr(cond, targets);
                collect_pre_targets_from_expr(then_expr, targets);
            }
            collect_pre_targets_from_expr(else_branch, targets);
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            for elem in elements {
                collect_pre_targets_from_expr(elem, targets);
            }
        }
        dae::Expression::Range { start, step, end } => {
            collect_pre_targets_from_expr(start, targets);
            if let Some(s) = step {
                collect_pre_targets_from_expr(s, targets);
            }
            collect_pre_targets_from_expr(end, targets);
        }
        dae::Expression::Index { base, .. } | dae::Expression::FieldAccess { base, .. } => {
            collect_pre_targets_from_expr(base, targets);
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            collect_pre_targets_from_expr(expr, targets);
            for idx in indices {
                collect_pre_targets_from_expr(&idx.range, targets);
            }
            if let Some(f) = filter {
                collect_pre_targets_from_expr(f, targets);
            }
        }
        dae::Expression::VarRef { .. } | dae::Expression::Literal(_) | dae::Expression::Empty => {}
    }
}

fn collect_all_variables(dae: &dae::Dae) -> HashMap<dae::VarName, &dae::Variable> {
    let mut all = HashMap::new();
    for (name, var) in &dae.states {
        all.insert(name.clone(), var);
    }
    for (name, var) in &dae.algebraics {
        all.insert(name.clone(), var);
    }
    for (name, var) in &dae.inputs {
        all.insert(name.clone(), var);
    }
    for (name, var) in &dae.outputs {
        all.insert(name.clone(), var);
    }
    for (name, var) in &dae.parameters {
        all.insert(name.clone(), var);
    }
    for (name, var) in &dae.constants {
        all.insert(name.clone(), var);
    }
    for (name, var) in &dae.discrete_reals {
        all.insert(name.clone(), var);
    }
    for (name, var) in &dae.discrete_valued {
        all.insert(name.clone(), var);
    }
    all
}

fn rewrite_equations(equations: &mut [dae::Equation], targets: &IndexMap<dae::VarName, PreTarget>) {
    for eq in equations {
        eq.rhs = rewrite_pre_expr(
            std::mem::replace(&mut eq.rhs, dae::Expression::Empty),
            targets,
        );
    }
}

fn rewrite_pre_expr(
    expr: dae::Expression,
    targets: &IndexMap<dae::VarName, PreTarget>,
) -> dae::Expression {
    match expr {
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Pre,
            args,
        } => rewrite_pre_builtin(args, targets),
        other => rewrite_non_pre_expr(other, targets),
    }
}

fn rewrite_pre_builtin(
    args: Vec<dae::Expression>,
    targets: &IndexMap<dae::VarName, PreTarget>,
) -> dae::Expression {
    let rewritten_args: Vec<dae::Expression> = args
        .into_iter()
        .map(|a| rewrite_pre_expr(a, targets))
        .collect();
    if let Some(dae::Expression::VarRef { name, subscripts }) = rewritten_args.first()
        && targets.contains_key(name)
    {
        let pre_name = dae::VarName::new(format!("__pre__.{}", name.as_str()));
        return dae::Expression::VarRef {
            name: pre_name,
            subscripts: subscripts.clone(),
        };
    }
    // Preserve unsupported pre(...) shapes so validation/errors can surface
    // instead of silently dropping expression structure.
    dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Pre,
        args: rewritten_args,
    }
}

fn rewrite_non_pre_expr(
    expr: dae::Expression,
    targets: &IndexMap<dae::VarName, PreTarget>,
) -> dae::Expression {
    match expr {
        dae::Expression::Binary { op, lhs, rhs } => dae::Expression::Binary {
            op,
            lhs: Box::new(rewrite_pre_expr(*lhs, targets)),
            rhs: Box::new(rewrite_pre_expr(*rhs, targets)),
        },
        dae::Expression::Unary { op, rhs } => dae::Expression::Unary {
            op,
            rhs: Box::new(rewrite_pre_expr(*rhs, targets)),
        },
        dae::Expression::BuiltinCall { function, args } => dae::Expression::BuiltinCall {
            function,
            args: args
                .into_iter()
                .map(|a| rewrite_pre_expr(a, targets))
                .collect(),
        },
        dae::Expression::FunctionCall {
            name,
            args,
            is_constructor,
        } => dae::Expression::FunctionCall {
            name,
            args: args
                .into_iter()
                .map(|a| rewrite_pre_expr(a, targets))
                .collect(),
            is_constructor,
        },
        dae::Expression::If {
            branches,
            else_branch,
        } => dae::Expression::If {
            branches: branches
                .into_iter()
                .map(|(c, t)| (rewrite_pre_expr(c, targets), rewrite_pre_expr(t, targets)))
                .collect(),
            else_branch: Box::new(rewrite_pre_expr(*else_branch, targets)),
        },
        dae::Expression::Array {
            elements,
            is_matrix,
        } => dae::Expression::Array {
            elements: elements
                .into_iter()
                .map(|e| rewrite_pre_expr(e, targets))
                .collect(),
            is_matrix,
        },
        dae::Expression::Tuple { elements } => dae::Expression::Tuple {
            elements: elements
                .into_iter()
                .map(|e| rewrite_pre_expr(e, targets))
                .collect(),
        },
        dae::Expression::Range { start, step, end } => dae::Expression::Range {
            start: Box::new(rewrite_pre_expr(*start, targets)),
            step: step.map(|s| Box::new(rewrite_pre_expr(*s, targets))),
            end: Box::new(rewrite_pre_expr(*end, targets)),
        },
        dae::Expression::Index { base, subscripts } => dae::Expression::Index {
            base: Box::new(rewrite_pre_expr(*base, targets)),
            subscripts,
        },
        dae::Expression::FieldAccess { base, field } => dae::Expression::FieldAccess {
            base: Box::new(rewrite_pre_expr(*base, targets)),
            field,
        },
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => dae::Expression::ArrayComprehension {
            expr: Box::new(rewrite_pre_expr(*expr, targets)),
            indices,
            filter: filter.map(|f| Box::new(rewrite_pre_expr(*f, targets))),
        },
        other @ (dae::Expression::VarRef { .. }
        | dae::Expression::Literal(_)
        | dae::Expression::Empty) => other,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn var_ref(name: &str) -> dae::Expression {
        dae::Expression::VarRef {
            name: dae::VarName::new(name),
            subscripts: vec![],
        }
    }

    fn pre_call(name: &str) -> dae::Expression {
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Pre,
            args: vec![var_ref(name)],
        }
    }

    #[test]
    fn test_lower_pre_creates_parameter() {
        let mut dae = dae::Dae::new();
        dae.discrete_reals.insert(
            dae::VarName::new("pulse_count"),
            dae::Variable {
                name: dae::VarName::new("pulse_count"),
                dims: vec![],
                start: Some(dae::Expression::Literal(dae::Literal::Real(0.0))),
                fixed: None,
                min: None,
                max: None,
                nominal: None,
                unit: None,
                state_select: rumoca_ir_core::StateSelect::Default,
                description: None,
            },
        );
        dae.f_x.push(dae::Equation::residual(
            pre_call("pulse_count"),
            rumoca_core::Span::default(),
            "test".to_string(),
        ));

        lower_pre_operator(&mut dae);

        // Should have created a parameter __pre__.pulse_count
        assert!(
            dae.parameters
                .contains_key(&dae::VarName::new("__pre__.pulse_count"))
        );
        let pre_param = &dae.parameters[&dae::VarName::new("__pre__.pulse_count")];
        assert_eq!(
            pre_param.start,
            Some(dae::Expression::Literal(dae::Literal::Real(0.0)))
        );

        // The equation should now reference __pre__.pulse_count
        match &dae.f_x[0].rhs {
            dae::Expression::VarRef { name, .. } => {
                assert_eq!(name.as_str(), "__pre__.pulse_count");
            }
            other => panic!("Expected VarRef, got {:?}", other),
        }
    }

    #[test]
    fn test_lower_pre_no_pre_calls_is_noop() {
        let mut dae = dae::Dae::new();
        dae.f_x.push(dae::Equation::residual(
            var_ref("x"),
            rumoca_core::Span::default(),
            "test".to_string(),
        ));

        lower_pre_operator(&mut dae);

        assert!(dae.parameters.is_empty());
    }

    #[test]
    fn test_lower_pre_preserves_unhandled_pre_shape() {
        let mut dae = dae::Dae::new();
        dae.f_x.push(dae::Equation::residual(
            dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Pre,
                args: vec![dae::Expression::Literal(dae::Literal::Integer(1))],
            },
            rumoca_core::Span::default(),
            "test".to_string(),
        ));

        lower_pre_operator(&mut dae);

        assert!(matches!(
            &dae.f_x[0].rhs,
            dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Pre,
                ..
            }
        ));
    }
}
