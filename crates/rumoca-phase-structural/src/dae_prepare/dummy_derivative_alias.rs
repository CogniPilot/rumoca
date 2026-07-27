//! Structural elimination of algebraic aliases for state derivatives.
//!
//! A finalized DAE may contain a scalar algebraic `di` defined by
//! `di = der(x)` and also use `der(x)` in constitutive equations. The
//! structural form must retain the defining row as the unique derivative row
//! and substitute `der(x) -> di` everywhere else. This is a DAE-to-DAE
//! structural transformation; Solve lowering must only consume its result.

use std::collections::{HashMap, HashSet};

use rumoca_core::{
    BuiltinFunction, Expression, ExpressionRewriter, Reference, Span, Subscript, VarName,
};
use rumoca_ir_dae as dae;

/// Return a structurally rewritten DAE when scalar dummy-derivative aliases are
/// present.
///
/// The defining `dummy = der(state)` row remains unchanged. Every other
/// continuous equation substitutes `der(state)` with the dummy algebraic so
/// the defining row becomes the unique state-derivative row and constitutive
/// equations determine the dummy.
#[must_use]
pub fn eliminate_dummy_derivative_aliases(dae_model: &dae::Dae) -> Option<dae::Dae> {
    // The copy-returning form is the one that pays for a copy, and it pays for
    // it only when there is something to rewrite: the alias scan is read-only,
    // so it runs on the borrowed DAE first.
    if collect_dummy_derivative_aliases(dae_model)
        .state_to_dummy
        .is_empty()
    {
        return None;
    }
    let mut rewritten = super::copy_accounting::clone_dae(dae_model);
    eliminate_dummy_derivative_aliases_in_place(&mut rewritten).then_some(rewritten)
}

/// Apply the same rewrite at the structural DAE boundary, without a copy.
///
/// Returns `true` when the DAE changed. This is the form the structural funnel
/// uses, and it rewrites the equations it owns directly: the rewrite has no
/// failure mode, so there is nothing for a staging copy to roll back to. The
/// copy-returning form above remains useful to Solve entry points that accept
/// only a shared DAE reference.
pub fn eliminate_dummy_derivative_aliases_in_place(dae_model: &mut dae::Dae) -> bool {
    let aliases = collect_dummy_derivative_aliases(dae_model);
    if aliases.state_to_dummy.is_empty() {
        return false;
    }

    let mut rewriter = DerToDummyRewriter {
        state_to_dummy: &aliases.state_to_dummy,
    };
    for (index, equation) in dae_model.continuous.equations.iter_mut().enumerate() {
        if aliases.defining_equation_indices.contains(&index) {
            continue;
        }
        equation.rhs = rewriter.rewrite_expression(&equation.rhs);
    }
    for family in &mut dae_model.continuous.structured_equations {
        let Some(template) = family.template.as_mut() else {
            continue;
        };
        for (equation_position, body) in template.body.iter_mut().enumerate() {
            let is_defining_body = family
                .first_equation_index
                .checked_add(equation_position)
                .is_some_and(|index| aliases.defining_equation_indices.contains(&index));
            if !is_defining_body {
                *body = rewriter.rewrite_expression(body);
            }
        }
    }
    true
}

struct DummyDerivativeAliases {
    state_to_dummy: HashMap<VarName, VarName>,
    defining_equation_indices: HashSet<usize>,
}

fn collect_dummy_derivative_aliases(dae_model: &dae::Dae) -> DummyDerivativeAliases {
    let mut state_to_dummy = HashMap::new();
    let mut dummy_used = HashSet::new();
    let mut defining_equation_indices = HashSet::new();

    for (index, equation) in dae_model.continuous.equations.iter().enumerate() {
        let Some((state, dummy)) = dummy_definition(dae_model, equation) else {
            continue;
        };
        if state_to_dummy.contains_key(&state) || dummy_used.contains(&dummy) {
            continue;
        }
        state_to_dummy.insert(state, dummy.clone());
        dummy_used.insert(dummy);
        defining_equation_indices.insert(index);
    }

    DummyDerivativeAliases {
        state_to_dummy,
        defining_equation_indices,
    }
}

fn dummy_definition(dae_model: &dae::Dae, equation: &dae::Equation) -> Option<(VarName, VarName)> {
    if let Some(lhs) = equation.lhs.as_ref() {
        let dummy = scalar_algebraic_name(dae_model, lhs.var_name())?;
        let state = der_of_state(dae_model, &equation.rhs)?;
        return Some((state, dummy));
    }
    let (lhs, rhs) = split_subtraction(&equation.rhs)?;
    if let Some(dummy) = scalar_algebraic_expr(dae_model, lhs)
        && let Some(state) = der_of_state(dae_model, rhs)
    {
        return Some((state, dummy));
    }
    if let Some(dummy) = scalar_algebraic_expr(dae_model, rhs)
        && let Some(state) = der_of_state(dae_model, lhs)
    {
        return Some((state, dummy));
    }
    None
}

fn split_subtraction(expression: &Expression) -> Option<(&Expression, &Expression)> {
    match expression {
        Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs,
            rhs,
            ..
        } => Some((lhs, rhs)),
        _ => None,
    }
}

fn der_of_state(dae_model: &dae::Dae, expression: &Expression) -> Option<VarName> {
    let Expression::BuiltinCall {
        function: BuiltinFunction::Der,
        args,
        ..
    } = expression
    else {
        return None;
    };
    let [argument] = args.as_slice() else {
        return None;
    };
    let name = scalar_var_ref_name(argument)?;
    dae_model
        .variables
        .states
        .contains_key(&name)
        .then_some(name)
}

fn scalar_algebraic_expr(dae_model: &dae::Dae, expression: &Expression) -> Option<VarName> {
    scalar_algebraic_name(dae_model, &scalar_var_ref_name(expression)?)
}

fn scalar_algebraic_name(dae_model: &dae::Dae, name: &VarName) -> Option<VarName> {
    dae_model
        .variables
        .algebraics
        .contains_key(name)
        .then(|| name.clone())
}

fn scalar_var_ref_name(expression: &Expression) -> Option<VarName> {
    match expression {
        Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => Some(name.var_name().clone()),
        _ => None,
    }
}

struct DerToDummyRewriter<'a> {
    state_to_dummy: &'a HashMap<VarName, VarName>,
}

impl ExpressionRewriter for DerToDummyRewriter<'_> {
    fn walk_builtin_call_expression(
        &mut self,
        function: BuiltinFunction,
        args: &[Expression],
        span: Span,
    ) -> Expression {
        if function == BuiltinFunction::Der
            && let [argument] = args
            && let Some(state) = scalar_var_ref_name(argument)
            && let Some(dummy) = self.state_to_dummy.get(&state)
        {
            return Expression::VarRef {
                name: Reference::from_var_name(dummy.clone()),
                subscripts: Vec::<Subscript>::new(),
                span,
            };
        }
        Expression::BuiltinCall {
            function,
            args: args
                .iter()
                .map(|argument| self.rewrite_expression(argument))
                .collect(),
            span,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::{
        ComprehensionScalarView, ComprehensionTemplate, SourceId, StructuredIndexBinder,
        StructuredIndexDomain,
    };

    fn span() -> Span {
        Span::from_offsets(
            SourceId::from_source_name("dummy_derivative_alias.mo"),
            1,
            2,
        )
    }

    fn var_ref(name: &str) -> Expression {
        Expression::VarRef {
            name: Reference::new(name),
            subscripts: Vec::new(),
            span: span(),
        }
    }

    fn der(name: &str) -> Expression {
        Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            args: vec![var_ref(name)],
            span: span(),
        }
    }

    fn equation(lhs: Option<&str>, rhs: Expression) -> dae::Equation {
        dae::Equation {
            lhs: lhs.map(Reference::new),
            rhs,
            span: span(),
            origin: "fixture".to_string(),
            scalar_count: 1,
        }
    }

    #[test]
    fn rewrites_nondefining_rows_and_templates_at_the_structural_boundary() {
        let mut model = dae::Dae::new();
        model.variables.states.insert(
            VarName::new("x"),
            dae::Variable {
                name: VarName::new("x"),
                ..dae::Variable::empty_with_span(span())
            },
        );
        model.variables.algebraics.insert(
            VarName::new("dummy"),
            dae::Variable {
                name: VarName::new("dummy"),
                ..dae::Variable::empty_with_span(span())
            },
        );
        model.continuous.equations =
            vec![equation(Some("dummy"), der("x")), equation(None, der("x"))];
        model
            .continuous
            .structured_equations
            .push(dae::StructuredEquationFamily {
                domain: StructuredIndexDomain {
                    binders: vec![StructuredIndexBinder {
                        id: 0,
                        display_name: "i".to_string(),
                        lower: 1,
                        upper: 1,
                        step: 1,
                    }],
                },
                first_equation_index: 0,
                equations_per_point: 2,
                span: span(),
                origin: "fixture family".to_string(),
                regular: None,
                template: Some(ComprehensionTemplate {
                    body: vec![der("x"), der("x")],
                    scalar_view: ComprehensionScalarView::BinderSubstitution,
                }),
                interiors_materialized: true,
            });

        assert!(eliminate_dummy_derivative_aliases_in_place(&mut model));

        assert!(matches!(
            model.continuous.equations[0].rhs,
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                ..
            }
        ));
        assert!(matches!(
            &model.continuous.equations[1].rhs,
            Expression::VarRef { name, .. } if name.as_str() == "dummy"
        ));
        let body = &model.continuous.structured_equations[0]
            .template
            .as_ref()
            .expect("fixture template remains present")
            .body;
        assert!(matches!(
            body[0],
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                ..
            }
        ));
        assert!(matches!(
            &body[1],
            Expression::VarRef { name, .. } if name.as_str() == "dummy"
        ));
    }
}
