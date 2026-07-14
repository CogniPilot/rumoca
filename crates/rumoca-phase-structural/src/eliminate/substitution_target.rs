use rumoca_core::{
    BuiltinFunction, ComponentRefPart, ComponentReference, Expression, ExpressionVisitor,
    Reference, Subscript, component_reference_from_flat_name,
};

use super::{
    Dae, Substitution, aggregate_subscript_ref_matches_var, der_call_matches_scalar_substitution,
    embedded_alias_indices_for_substitution, var_ref_matches_unknown_for_substitution_in_scope,
};
use crate::variable_scope::DaeVariableScope;

pub(super) fn expression_is_exact_structured_substitution_target(
    dae: &Dae,
    expr: &Expression,
    substitution: &Substitution,
) -> bool {
    let Some(target) = DaeVariableScope::new(dae).exact(&substitution.var_name) else {
        return false;
    };
    let Some(target_ref) = target.component_ref.as_ref() else {
        return false;
    };
    let Some(reference) = structured_reference_from_expr(expr) else {
        return false;
    };
    if !component_reference_path_matches(&reference.component_ref, target_ref)
        || unique_structured_target_name(dae, target_ref).as_ref() != Some(&substitution.var_name)
    {
        return false;
    }
    !reference.terminal_identity_complete
        || match (reference.component_ref.def_id, target_ref.def_id) {
            (Some(expr_def_id), Some(target_def_id)) => expr_def_id == target_def_id,
            (None, _) => true,
            (Some(_), None) => false,
        }
}

pub(super) fn substitution_requires_structured_identity(
    dae: &Dae,
    substitution: &Substitution,
) -> bool {
    substitution.var_dims.is_empty()
        && DaeVariableScope::new(dae)
            .exact(&substitution.var_name)
            .is_some_and(|var| var.component_ref.is_some())
}

struct StructuredExpressionReference {
    component_ref: ComponentReference,
    terminal_identity_complete: bool,
}

fn structured_reference_from_expr(expr: &Expression) -> Option<StructuredExpressionReference> {
    match expr {
        Expression::VarRef {
            name,
            subscripts,
            span,
        } => {
            let mut component_ref = name
                .component_ref()
                .cloned()
                .or_else(|| component_reference_from_flat_name(name.var_name(), *span))?;
            append_exact_subscripts(&mut component_ref, subscripts)?;
            Some(StructuredExpressionReference {
                component_ref,
                terminal_identity_complete: subscripts.is_empty(),
            })
        }
        Expression::Index {
            base, subscripts, ..
        } => {
            let mut reference = structured_reference_from_expr(base)?;
            append_exact_subscripts(&mut reference.component_ref, subscripts)?;
            reference.terminal_identity_complete = false;
            Some(reference)
        }
        Expression::FieldAccess { base, field, span } => {
            let mut reference = structured_reference_from_expr(base)?;
            reference.component_ref.parts.push(ComponentRefPart {
                ident: field.clone(),
                span: *span,
                subs: Vec::new(),
            });
            reference.terminal_identity_complete = false;
            Some(reference)
        }
        _ => None,
    }
}

fn append_exact_subscripts(
    component_ref: &mut ComponentReference,
    subscripts: &[Subscript],
) -> Option<()> {
    let part = component_ref.parts.last_mut()?;
    for subscript in subscripts {
        let Subscript::Index { value, span } = subscript else {
            return None;
        };
        part.subs.push(Subscript::Index {
            value: *value,
            span: *span,
        });
    }
    Some(())
}

fn component_reference_path_matches(
    expression: &ComponentReference,
    target: &ComponentReference,
) -> bool {
    expression.local == target.local
        && expression.parts.len() == target.parts.len()
        && expression
            .parts
            .iter()
            .zip(&target.parts)
            .all(|(expression, target)| {
                expression.ident == target.ident
                    && exact_subscript_values(&expression.subs)
                        == exact_subscript_values(&target.subs)
            })
}

fn exact_subscript_values(subscripts: &[Subscript]) -> Option<Vec<i64>> {
    subscripts
        .iter()
        .map(|subscript| match subscript {
            Subscript::Index { value, .. } => Some(*value),
            Subscript::Colon { .. } | Subscript::Expr { .. } => None,
        })
        .collect()
}

fn unique_structured_target_name(
    dae: &Dae,
    target: &ComponentReference,
) -> Option<rumoca_core::VarName> {
    let variables = &dae.variables;
    let mut matches = variables
        .states
        .values()
        .chain(variables.algebraics.values())
        .chain(variables.inputs.values())
        .chain(variables.outputs.values())
        .chain(variables.parameters.values())
        .chain(variables.constants.values())
        .chain(variables.discrete_reals.values())
        .chain(variables.discrete_valued.values())
        .filter(|variable| {
            variable
                .component_ref
                .as_ref()
                .is_some_and(|candidate| component_reference_path_matches(candidate, target))
        });
    let name = matches.next()?.name.clone();
    matches.next().is_none().then_some(name)
}

pub(super) fn expr_contains_substitution_target_in_scope(
    expr: &Expression,
    substitution: &Substitution,
    dae_scope: Option<&DaeVariableScope<'_>>,
    dae_context: Option<&Dae>,
) -> bool {
    let mut checker = SubstitutionTargetChecker {
        substitution,
        dae_scope,
        dae_context,
        structured_identity_required: dae_context
            .is_some_and(|dae| substitution_requires_structured_identity(dae, substitution)),
        found: false,
    };
    checker.visit_expression(expr);
    checker.found
}

struct SubstitutionTargetChecker<'a> {
    substitution: &'a Substitution,
    dae_scope: Option<&'a DaeVariableScope<'a>>,
    dae_context: Option<&'a Dae>,
    structured_identity_required: bool,
    found: bool,
}

impl ExpressionVisitor for SubstitutionTargetChecker<'_> {
    fn visit_expression(&mut self, expr: &Expression) {
        if self.found {
            return;
        }
        if self.dae_context.is_some_and(|dae| {
            expression_is_exact_structured_substitution_target(dae, expr, self.substitution)
        }) {
            self.found = true;
            return;
        }
        self.walk_expression(expr);
    }

    fn visit_var_ref(&mut self, name: &Reference, subscripts: &[rumoca_core::Subscript]) {
        if self.structured_identity_required {
            for subscript in subscripts {
                self.visit_subscript(subscript);
            }
            return;
        }
        if aggregate_subscript_ref_matches_var(name, subscripts, self.substitution)
            || var_ref_matches_unknown_for_substitution_in_scope(
                name,
                subscripts,
                self.substitution,
                self.dae_scope,
            )
            || embedded_alias_indices_for_substitution(name, subscripts, self.substitution)
                .is_some()
        {
            self.found = true;
            return;
        }
        for subscript in subscripts {
            self.visit_subscript(subscript);
        }
    }
}

pub(super) fn expr_contains_derivative_substitution_target(
    expr: &Expression,
    substitution: &Substitution,
) -> bool {
    let mut checker = DerivativeSubstitutionTargetChecker {
        substitution,
        found: false,
    };
    checker.visit_expression(expr);
    checker.found
}

struct DerivativeSubstitutionTargetChecker<'a> {
    substitution: &'a Substitution,
    found: bool,
}

impl ExpressionVisitor for DerivativeSubstitutionTargetChecker<'_> {
    fn visit_expression(&mut self, expr: &Expression) {
        if self.found {
            return;
        }
        match expr {
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args,
                ..
            } if der_call_matches_scalar_substitution(args, self.substitution) => {
                self.found = true;
            }
            _ => self.walk_expression(expr),
        }
    }
}
