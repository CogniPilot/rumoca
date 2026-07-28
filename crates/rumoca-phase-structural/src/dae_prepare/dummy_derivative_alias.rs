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
#[must_use = "the optional rewritten DAE must replace the input seen by Solve lowering"]
pub fn eliminate_dummy_derivative_aliases(
    dae_model: &dae::Dae,
) -> Result<Option<dae::Dae>, crate::StructuralError> {
    // The copy-returning form is the one that pays for a copy, and it pays for
    // it only when there is something to rewrite: the alias scan is read-only,
    // so it runs on the borrowed DAE first.
    if collect_dummy_derivative_aliases(dae_model)
        .state_to_dummy
        .is_empty()
    {
        return Ok(None);
    }
    let mut rewritten = super::copy_accounting::clone_dae(dae_model);
    if !eliminate_dummy_derivative_aliases_in_place(&mut rewritten) {
        return Ok(None);
    }
    crate::finish_dae(rewritten).map(Some)
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
    let structural_bindings = crate::static_eval::structural_scalar_bindings(dae_model);

    for (index, equation) in dae_model.continuous.equations.iter().enumerate() {
        let Some((state, dummy)) = dummy_definition(dae_model, equation, &structural_bindings)
        else {
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

fn dummy_definition(
    dae_model: &dae::Dae,
    equation: &dae::Equation,
    structural_bindings: &HashMap<String, f64>,
) -> Option<(VarName, VarName)> {
    if let Some(lhs) = equation.lhs.as_ref() {
        let dummy = scalar_algebraic_name(dae_model, lhs.var_name())?;
        let state = unit_scaled_der_of_state(dae_model, &equation.rhs, structural_bindings)?;
        return Some((state, dummy));
    }
    let (lhs, rhs) = split_subtraction(&equation.rhs)?;
    if let Some(dummy) = scalar_algebraic_expr(dae_model, lhs)
        && let Some(state) = unit_scaled_der_of_state(dae_model, rhs, structural_bindings)
    {
        return Some((state, dummy));
    }
    if let Some(dummy) = scalar_algebraic_expr(dae_model, rhs)
        && let Some(state) = unit_scaled_der_of_state(dae_model, lhs, structural_bindings)
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

/// Prove that an expression is exactly `der(state)` after compile-time scalar
/// factors are evaluated.
///
/// Symbolic differentiation can preserve unit arithmetic such as
/// `(-der(x) * -1) / (-1 * -1)`. Treating this as a derivative alias is sound
/// only when the expression has one derivative leaf and its accumulated
/// coefficient is finite and exactly `+1`.
fn unit_scaled_der_of_state(
    dae_model: &dae::Dae,
    expression: &Expression,
    structural_bindings: &HashMap<String, f64>,
) -> Option<VarName> {
    let (state, coefficient) = scaled_der_of_state(dae_model, expression, structural_bindings)?;
    (coefficient.is_finite() && coefficient == 1.0).then_some(state)
}

fn scaled_der_of_state(
    dae_model: &dae::Dae,
    expression: &Expression,
    structural_bindings: &HashMap<String, f64>,
) -> Option<(VarName, f64)> {
    if let Some(state) = der_of_state(dae_model, expression) {
        return Some((state, 1.0));
    }
    match expression {
        Expression::Unary { op, rhs, .. } => {
            let (state, coefficient) = scaled_der_of_state(dae_model, rhs, structural_bindings)?;
            match op {
                rumoca_core::OpUnary::Plus
                | rumoca_core::OpUnary::DotPlus
                | rumoca_core::OpUnary::Empty => Some((state, coefficient)),
                rumoca_core::OpUnary::Minus | rumoca_core::OpUnary::DotMinus => {
                    Some((state, -coefficient))
                }
                rumoca_core::OpUnary::Not => None,
            }
        }
        Expression::Binary { op, lhs, rhs, .. } => {
            scaled_der_binary(dae_model, op.clone(), lhs, rhs, structural_bindings)
        }
        _ => None,
    }
}

fn scaled_der_binary(
    dae_model: &dae::Dae,
    op: rumoca_core::OpBinary,
    lhs: &Expression,
    rhs: &Expression,
    structural_bindings: &HashMap<String, f64>,
) -> Option<(VarName, f64)> {
    use rumoca_core::OpBinary;

    let lhs_derivative = scaled_der_of_state(dae_model, lhs, structural_bindings);
    let rhs_derivative = scaled_der_of_state(dae_model, rhs, structural_bindings);
    match (op, lhs_derivative, rhs_derivative) {
        (OpBinary::Mul | OpBinary::MulElem, Some((state, coefficient)), None) => Some((
            state,
            coefficient * crate::static_eval::eval_static_number(rhs, structural_bindings)?,
        )),
        (OpBinary::Mul | OpBinary::MulElem, None, Some((state, coefficient))) => Some((
            state,
            crate::static_eval::eval_static_number(lhs, structural_bindings)? * coefficient,
        )),
        (OpBinary::Div | OpBinary::DivElem, Some((state, coefficient)), None) => Some((
            state,
            coefficient / crate::static_eval::eval_static_number(rhs, structural_bindings)?,
        )),
        _ => None,
    }
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

    fn real(value: f64) -> Expression {
        Expression::Literal {
            value: rumoca_core::Literal::Real(value),
            span: span(),
        }
    }

    fn binary(op: rumoca_core::OpBinary, lhs: Expression, rhs: Expression) -> Expression {
        Expression::Binary {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
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

    #[test]
    fn proves_generated_unit_scaled_derivative_alias_before_rewriting() {
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
        let negative_derivative = Expression::Unary {
            op: rumoca_core::OpUnary::Minus,
            rhs: Box::new(der("x")),
            span: span(),
        };
        let generated_unit_derivative = binary(
            rumoca_core::OpBinary::Div,
            binary(rumoca_core::OpBinary::Mul, negative_derivative, real(-1.0)),
            binary(rumoca_core::OpBinary::Mul, real(-1.0), real(-1.0)),
        );
        model.continuous.equations = vec![
            equation(Some("dummy"), generated_unit_derivative),
            equation(None, der("x")),
        ];

        assert!(eliminate_dummy_derivative_aliases_in_place(&mut model));
        assert!(matches!(
            &model.continuous.equations[1].rhs,
            Expression::VarRef { name, .. } if name.as_str() == "dummy"
        ));
    }

    #[test]
    fn rejects_non_unit_scaled_derivative_alias() {
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
        model.continuous.equations = vec![
            equation(
                Some("dummy"),
                binary(rumoca_core::OpBinary::Mul, real(2.0), der("x")),
            ),
            equation(None, der("x")),
        ];

        assert!(!eliminate_dummy_derivative_aliases_in_place(&mut model));
        assert!(matches!(
            model.continuous.equations[1].rhs,
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                ..
            }
        ));
    }
}
