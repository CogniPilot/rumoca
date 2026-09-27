//! Parameters whose conditional guards DAE construction evaluates at
//! translation (SPEC_0040 DAE-C22, SPEC_0044 ME-PARAM-001).
//!
//! An equation conditional whose guard reads an ordinary parameter stays a
//! run-time branch when its arms are structurally equal; otherwise equation
//! lowering folds it as a structural selection. A variable's attribute or
//! binding value folds such a guard only when an arm calls a user function. A
//! folded guard freezes the parameter at its translation-time value, so a
//! later set of it could not take effect. Each such parameter, and every
//! parameter its binding reads, is therefore evaluable: fixed at translation
//! and exported non-settable.

use std::collections::HashSet;

use rumoca_core::{Expression, ExpressionVisitor};

use super::super::expression::conditional_guards::{
    conditional_calls_a_user_function, retains_flat_guard,
};
use super::super::function_shapes::{ProvenValue, ShapeEnvironment};
use super::{ValueReads, VarName, Variability, flat};

/// One owner whose folded guard fixes parameters at translation: the equation
/// or declaration it appears in and the parameters its guard reads, for the
/// SPEC_0040 DAE-C22 warning.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StructuralSelection {
    pub span: rumoca_core::Span,
    pub parameters: Vec<String>,
}

/// The ordinary parameters a folded guard reads, closed over the parameters
/// their bindings read, and the owners that fold them. `evaluable` is the
/// translation-time set already known.
pub(super) fn folded_guard_parameters(
    flat: &flat::Model,
    values: &ShapeEnvironment,
    evaluable: &HashSet<VarName>,
) -> (HashSet<VarName>, Vec<StructuralSelection>) {
    let mut scan = GuardScan {
        flat,
        values,
        evaluable,
        attribute_scope: false,
        owner: None,
        found: HashSet::new(),
        selections: Vec::new(),
        if_span: None,
    };
    for equation in flat.equations.iter().chain(&flat.initial_equations) {
        scan.owner = Some(equation.span);
        scan.visit_expression(&equation.residual);
    }
    for variable in flat.variables.values() {
        // A parameter or constant binding lowers as a value (attribute scope);
        // any other declaration binding lowers as its equation.
        scan.owner = Some(variable.source_span);
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
    for selection in &flat.parameter_branch_selections {
        let read = flatten_selection_parameters(flat, evaluable, selection);
        if !read.is_empty() {
            scan.owner = Some(selection.span);
            scan.record(read);
        }
    }
    let selections = scan.selections;
    (close_over_bindings(flat, evaluable, scan.found), selections)
}

struct GuardScan<'a> {
    flat: &'a flat::Model,
    values: &'a ShapeEnvironment,
    evaluable: &'a HashSet<VarName>,
    attribute_scope: bool,
    /// The equation or declaration being scanned.
    owner: Option<rumoca_core::Span>,
    found: HashSet<VarName>,
    selections: Vec<StructuralSelection>,
    /// The span of the conditional being visited.
    if_span: Option<rumoca_core::Span>,
}

impl GuardScan<'_> {
    /// The ordinary parameters `expression` reads by value.
    fn ordinary_parameters(&self, expression: &Expression) -> Vec<VarName> {
        ordinary_parameters(self.flat, self.evaluable, expression)
    }

    /// Record parameters a folded guard of the current owner reads.
    fn record(&mut self, read: Vec<VarName>) {
        let mut parameters = read.iter().map(ToString::to_string).collect::<Vec<_>>();
        parameters.sort();
        parameters.dedup();
        if let Some(span) = self.owner {
            self.selections
                .push(StructuralSelection { span, parameters });
        }
        self.found.extend(read);
    }
}

impl ExpressionVisitor for GuardScan<'_> {
    fn visit_expression(&mut self, expression: &Expression) {
        if let Expression::If { span, .. } = expression {
            self.if_span = Some(*span);
        }
        self.walk_expression(expression);
    }

    fn visit_if(&mut self, branches: &[(Expression, Expression)], else_branch: &Expression) {
        let structural = self
            .if_span
            .take()
            .is_some_and(|span| self.values.is_structural_selection(span));
        let folds = if self.attribute_scope {
            conditional_calls_a_user_function(branches, else_branch)
        } else {
            structural || !retains_flat_guard(self.flat, self.evaluable, branches, else_branch)
        };
        for (condition, _) in branches {
            let read = self.ordinary_parameters(condition);
            if folds
                && !read.is_empty()
                && matches!(
                    self.values.proven_value(condition),
                    Some(ProvenValue::Boolean(_))
                )
            {
                self.record(read);
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

/// The ordinary parameters a flatten branch selection evaluated: for each
/// reference its conditions read, the innermost flat name the model declares,
/// when that is a parameter not already evaluable.
fn flatten_selection_parameters(
    flat: &flat::Model,
    evaluable: &HashSet<VarName>,
    selection: &flat::ParameterBranchSelection,
) -> Vec<VarName> {
    selection
        .references
        .iter()
        .filter_map(|candidates| {
            candidates
                .iter()
                .map(|candidate| VarName::new(candidate.as_str()))
                .find(|name| flat.variables.contains_key(name))
        })
        .filter(|name| {
            !evaluable.contains(name)
                && flat.variables.get(name).is_some_and(|variable| {
                    matches!(variable.variability, Variability::Parameter(_))
                })
        })
        .collect()
}
