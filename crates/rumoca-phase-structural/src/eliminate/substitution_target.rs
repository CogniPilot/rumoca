use rumoca_core::{BuiltinFunction, Expression, ExpressionVisitor, Reference};

use super::{
    Substitution, aggregate_subscript_ref_matches_var, der_call_matches_scalar_substitution,
    embedded_alias_indices_for_substitution, var_ref_matches_unknown_for_substitution,
};

pub(super) fn expr_contains_substitution_target(
    expr: &Expression,
    substitution: &Substitution,
) -> bool {
    let mut checker = SubstitutionTargetChecker {
        substitution,
        found: false,
    };
    checker.visit_expression(expr);
    checker.found
}

struct SubstitutionTargetChecker<'a> {
    substitution: &'a Substitution,
    found: bool,
}

impl ExpressionVisitor for SubstitutionTargetChecker<'_> {
    fn visit_expression(&mut self, expr: &Expression) {
        if !self.found {
            self.walk_expression(expr);
        }
    }

    fn visit_var_ref(&mut self, name: &Reference, subscripts: &[rumoca_core::Subscript]) {
        if aggregate_subscript_ref_matches_var(name, subscripts, self.substitution)
            || var_ref_matches_unknown_for_substitution(name, subscripts, self.substitution)
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
