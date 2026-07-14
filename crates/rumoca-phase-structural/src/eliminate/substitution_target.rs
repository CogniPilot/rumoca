use rumoca_core::{BuiltinFunction, Expression, ExpressionVisitor, Reference};

use super::{
    Dae, Substitution, aggregate_subscript_ref_matches_var, der_call_matches_scalar_substitution,
    embedded_alias_indices_for_substitution, exact_reference_expr_name_in_dae,
    var_ref_matches_unknown_for_substitution_in_scope,
};
use crate::variable_scope::DaeVariableScope;

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
        found: false,
    };
    checker.visit_expression(expr);
    checker.found
}

struct SubstitutionTargetChecker<'a> {
    substitution: &'a Substitution,
    dae_scope: Option<&'a DaeVariableScope<'a>>,
    dae_context: Option<&'a Dae>,
    found: bool,
}

impl ExpressionVisitor for SubstitutionTargetChecker<'_> {
    fn visit_expression(&mut self, expr: &Expression) {
        if self.found {
            return;
        }
        if self
            .dae_context
            .and_then(|dae| exact_reference_expr_name_in_dae(dae, expr))
            .as_ref()
            == Some(&self.substitution.var_name)
        {
            self.found = true;
            return;
        }
        self.walk_expression(expr);
    }

    fn visit_var_ref(&mut self, name: &Reference, subscripts: &[rumoca_core::Subscript]) {
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
