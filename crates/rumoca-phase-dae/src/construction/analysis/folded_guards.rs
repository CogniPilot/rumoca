//! Parameters whose conditional guards DAE construction evaluates at
//! translation (SPEC_0040 DAE-C22, SPEC_0044 ME-PARAM-001).
//!
//! An equation conditional whose guard reads an ordinary parameter stays a
//! run-time branch when its arms are structurally equal; otherwise it is a
//! structural selection, applied to the Flat model before analysis and
//! recorded as a Flat branch selection. A variable's attribute or
//! binding value folds such a guard when an arm calls a user function or its
//! arms are not proven to share one shape. A folded guard freezes the
//! parameter at its translation-time value, so a later set of it could not
//! take effect. Each such parameter, and every parameter its binding reads,
//! is therefore evaluable: fixed at translation and exported non-settable.

use std::collections::HashSet;

use rumoca_core::{Expression, ExpressionVisitor};

use super::super::expression::conditional_guards::attribute_conditional_folds;
use super::super::function_shapes::{ProvenValue, ShapeEnvironment};
use super::clocks::when_conditional_selects_clock_structure;
use super::{ValueReads, VarName, Variability, flat};

/// One owner whose folded guard fixes parameters at translation: the equation
/// or declaration it appears in and the parameters its guard reads, for the
/// SPEC_0040 DAE-C22 warning.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StructuralSelection {
    pub span: rumoca_core::Span,
    pub kind: flat::StructuralParameterUse,
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
        owner: None,
        found: HashSet::new(),
        selections: Vec::new(),
    };
    // Equation conditionals, which include every binding that is not a
    // parameter's or constant's, are selected before analysis and recorded as
    // Flat branch selections (`structural_selections`). A parameter or
    // constant binding and every start value lower as values (attribute
    // scope).
    for variable in flat.variables.values() {
        scan.owner = Some(variable.source_span);
        if matches!(
            variable.variability,
            Variability::Parameter(_) | Variability::Constant(_)
        ) && let Some(binding) = &variable.binding
        {
            scan.visit_expression(binding);
        }
        if let Some(start) = &variable.start {
            scan.visit_expression(start);
        }
    }
    for chain in &flat.when_chains {
        for branch in chain.branches() {
            scan.owner = Some(branch.span);
            scan.visit_clock_structure_conditionals(&branch.equations);
        }
    }
    for selection in &flat.parameter_branch_selections {
        let read = flatten_selection_parameters(flat, evaluable, selection);
        if !read.is_empty() {
            scan.owner = Some(selection.span);
            scan.record_use(selection.kind, read);
        }
    }
    let selections = scan.selections;
    (close_over_bindings(flat, evaluable, scan.found), selections)
}

struct GuardScan<'a> {
    flat: &'a flat::Model,
    values: &'a ShapeEnvironment,
    evaluable: &'a HashSet<VarName>,
    /// The declaration being scanned.
    owner: Option<rumoca_core::Span>,
    found: HashSet<VarName>,
    selections: Vec<StructuralSelection>,
}

impl GuardScan<'_> {
    /// The ordinary parameters `expression` reads by value.
    fn ordinary_parameters(&self, expression: &Expression) -> Vec<VarName> {
        ordinary_parameters(self.flat, self.evaluable, expression)
    }

    /// Record the guard parameters of every `when`-body conditional that
    /// selects clock structure: DAE construction decides it at translation
    /// (see [`when_conditional_selects_clock_structure`]).
    fn visit_clock_structure_conditionals(&mut self, equations: &[flat::WhenEquation]) {
        for equation in equations {
            let flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } = equation
            else {
                continue;
            };
            let decided =
                when_conditional_selects_clock_structure(branches, else_branch.as_deref());
            for (condition, _) in branches.iter().filter(|_| decided) {
                self.record_decided_guard(condition);
            }
            for (_, nested) in branches {
                self.visit_clock_structure_conditionals(nested);
            }
            if let Some(nested) = else_branch {
                self.visit_clock_structure_conditionals(nested);
            }
        }
    }

    /// Record the parameters of one guard the parameter values decide.
    fn record_decided_guard(&mut self, condition: &Expression) {
        let read = self.ordinary_parameters(condition);
        if !read.is_empty()
            && matches!(
                self.values.proven_value(condition),
                Some(ProvenValue::Boolean(_))
            )
        {
            self.record(read);
        }
    }

    /// Record parameters a folded guard of the current owner reads.
    fn record(&mut self, read: Vec<VarName>) {
        self.record_use(flat::StructuralParameterUse::BranchSelection, read);
    }

    /// Record parameters one structural use of the current owner reads, once
    /// per owner, use, and parameter set (a nested for-equation reports its
    /// range once, not once per enclosing iteration).
    fn record_use(&mut self, kind: flat::StructuralParameterUse, read: Vec<VarName>) {
        let mut parameters = read.iter().map(ToString::to_string).collect::<Vec<_>>();
        parameters.sort();
        parameters.dedup();
        if let Some(span) = self.owner {
            let selection = StructuralSelection {
                span,
                kind,
                parameters,
            };
            if !self.selections.contains(&selection) {
                self.selections.push(selection);
            }
        }
        self.found.extend(read);
    }
}

impl ExpressionVisitor for GuardScan<'_> {
    fn visit_if(&mut self, branches: &[(Expression, Expression)], else_branch: &Expression) {
        let folds = attribute_conditional_folds(branches, else_branch, self.values);
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

pub(super) fn ordinary_parameters(
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

/// Whether `name` is a `fixed = false` or `Evaluate = false` parameter.
fn non_evaluable_parameter(flat: &flat::Model, name: &VarName) -> bool {
    flat.variables.get(name).is_some_and(|variable| {
        variable.evaluate_refused
            || variable
                .fixed
                .as_ref()
                .is_some_and(|fixed| fixed.iter().any(|value| !value))
    })
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
        // MLS 3.7 sections 4.5 and 18.6: a `fixed = false` or
        // `Evaluate = false` parameter is never evaluable, so it is never
        // closed over; flatten refuses a structural use that reads one.
        if non_evaluable_parameter(flat, &name) || !closed.insert(name.clone()) {
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
