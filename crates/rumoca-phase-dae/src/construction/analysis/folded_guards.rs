//! Parameters whose conditional guards DAE construction evaluates at
//! translation (SPEC_0044 ME-PARAM-001, interim).
//!
//! Equation lowering folds an MLS §3.6.5 conditional whose guard this scope
//! proves, and a variable's attribute or binding value folds it only when an
//! arm calls a user function. A guard that reads an ordinary parameter is then
//! frozen at the parameter's translation-time value, so a later set of that
//! parameter could not take effect. Each such parameter, and every parameter
//! its binding reads, is therefore evaluable: fixed at translation and
//! exported non-settable.

use std::collections::HashSet;

use rumoca_core::{Expression, ExpressionVisitor};

use super::super::expression::conditional_guards::conditional_calls_a_user_function;
use super::super::function_shapes::{ProvenValue, ShapeEnvironment};
use super::{ValueReads, VarName, Variability, flat};

/// The ordinary parameters a folded guard reads, closed over the parameters
/// their bindings read. `evaluable` is the translation-time set already known.
pub(super) fn folded_guard_parameters(
    flat: &flat::Model,
    values: &ShapeEnvironment,
    evaluable: &HashSet<VarName>,
) -> HashSet<VarName> {
    let mut scan = GuardScan {
        flat,
        values,
        evaluable,
        attribute_scope: false,
        found: HashSet::new(),
    };
    for equation in flat.equations.iter().chain(&flat.initial_equations) {
        scan.visit_expression(&equation.residual);
    }
    for variable in flat.variables.values() {
        // A parameter or constant binding lowers as a value (attribute scope);
        // any other declaration binding lowers as its equation.
        scan.attribute_scope = matches!(
            variable.variability,
            Variability::Parameter(_) | Variability::Constant(_)
        );
        if let Some(binding) = &variable.binding {
            scan.visit_expression(binding);
        }
        scan.attribute_scope = true;
        if let Some(start) = &variable.start {
            scan.visit_expression(start);
        }
    }
    close_over_bindings(flat, evaluable, scan.found)
}

struct GuardScan<'a> {
    flat: &'a flat::Model,
    values: &'a ShapeEnvironment,
    evaluable: &'a HashSet<VarName>,
    attribute_scope: bool,
    found: HashSet<VarName>,
}

impl GuardScan<'_> {
    /// The ordinary parameters `expression` reads by value.
    fn ordinary_parameters(&self, expression: &Expression) -> Vec<VarName> {
        ordinary_parameters(self.flat, self.evaluable, expression)
    }
}

impl ExpressionVisitor for GuardScan<'_> {
    fn visit_if(&mut self, branches: &[(Expression, Expression)], else_branch: &Expression) {
        let folds =
            !self.attribute_scope || conditional_calls_a_user_function(branches, else_branch);
        for (condition, _) in branches {
            let read = self.ordinary_parameters(condition);
            if folds
                && !read.is_empty()
                && matches!(
                    self.values.proven_value(condition),
                    Some(ProvenValue::Boolean(_))
                )
            {
                self.found.extend(read);
            }
        }
        for (condition, value) in branches {
            self.visit_expression(condition);
            self.visit_expression(value);
        }
        self.visit_expression(else_branch);
    }
}

fn ordinary_parameters(
    flat: &flat::Model,
    evaluable: &HashSet<VarName>,
    expression: &Expression,
) -> Vec<VarName> {
    let mut reads = ValueReads::default();
    reads.visit_expression(expression);
    reads
        .names
        .into_iter()
        .filter(|name| {
            !evaluable.contains(name)
                && flat.variables.get(name).is_some_and(|variable| {
                    matches!(variable.variability, Variability::Parameter(_))
                })
        })
        .collect()
}

/// `found` with every ordinary parameter a member's binding reads, so each
/// member's binding reads only constants and evaluable parameters.
fn close_over_bindings(
    flat: &flat::Model,
    evaluable: &HashSet<VarName>,
    found: HashSet<VarName>,
) -> HashSet<VarName> {
    let mut closed = HashSet::new();
    let mut pending = found.into_iter().collect::<Vec<_>>();
    while let Some(name) = pending.pop() {
        if !closed.insert(name.clone()) {
            continue;
        }
        if let Some(binding) = flat
            .variables
            .get(&name)
            .and_then(|variable| variable.binding.as_ref())
        {
            pending.extend(ordinary_parameters(flat, evaluable, binding));
        }
    }
    closed
}
