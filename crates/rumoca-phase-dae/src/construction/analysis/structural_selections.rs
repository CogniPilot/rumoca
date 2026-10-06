//! The structurally selected Flat model (SPEC_0040 DAE-C22, MLS 3.7 §8.3.4).
//!
//! An equation conditional whose guard is not kept as a run-time branch is a
//! structural selection: an arm a proven condition never takes is not part of
//! the system of equations. The selection is made once, here, before any
//! analysis reads the equations, so roles, discrete partitions, balance, and
//! lowering all read the same selected rows. Each selection records the
//! ordinary parameters its decided conditions read as a Flat branch
//! selection, which fixes them at translation exactly as a selection made
//! during flattening does.

use std::borrow::Cow;

use rumoca_core::ExpressionRewriter;

use super::super::expression::conditional_guards::retains_flat_guard;
use super::super::function_shapes::ProvenValue;
use super::folded_guards::ordinary_parameters;
use super::*;

/// The model with every structural selection of its equations applied,
/// borrowed unchanged when no equation conditional is selected.
pub(in crate::construction) fn select_structural_branches<'a>(
    flat: Cow<'a, flat::Model>,
    values: &ShapeEnvironment,
) -> Cow<'a, flat::Model> {
    let Some(evaluable) = values.evaluable() else {
        return flat;
    };
    let mut selection = Selection {
        flat: flat.as_ref(),
        values,
        evaluable,
        decided: Vec::new(),
    };
    let equations = selected_equations(&mut selection, &flat.equations);
    let initial_equations = selected_equations(&mut selection, &flat.initial_equations);
    let families = selected_families(&mut selection, &flat.structured_equations);
    let initial_families = selected_families(&mut selection, &flat.initial_structured_equations);
    let bindings = selected_bindings(&mut selection, flat.as_ref());
    let selections = std::mem::take(&mut selection.decided);
    if equations.is_none()
        && initial_equations.is_none()
        && families.is_none()
        && initial_families.is_none()
        && bindings.is_empty()
    {
        return flat;
    }
    let mut selected = flat.into_owned();
    for (name, binding) in bindings {
        if let Some(variable) = selected.variables.get_mut(&name) {
            variable.binding = Some(binding);
        }
    }
    if let Some(equations) = equations {
        selected.equations = equations;
    }
    if let Some(equations) = initial_equations {
        selected.initial_equations = equations;
    }
    if let Some(families) = families {
        selected.structured_equations = families;
    }
    if let Some(families) = initial_families {
        selected.initial_structured_equations = families;
    }
    selected.parameter_branch_selections.extend(selections);
    Cow::Owned(selected)
}

/// Whether some equation-scope conditional of `flat` has a condition the
/// translation-time values can decide: one that reads, by value, only
/// parameters, constants, and enumeration literals. Only such a conditional
/// can be a structural selection.
pub(super) fn has_decidable_conditionals(flat: &flat::Model) -> bool {
    let templates = flat
        .structured_equations
        .iter()
        .chain(&flat.initial_structured_equations)
        .filter_map(|family| family.template.as_ref())
        .flat_map(|template| &template.body);
    let bindings = flat
        .variables
        .values()
        .filter(|variable| {
            !matches!(
                variable.variability,
                Variability::Parameter(_) | Variability::Constant(_)
            )
        })
        .filter_map(|variable| variable.binding.as_ref());
    flat.equations
        .iter()
        .chain(&flat.initial_equations)
        .map(|equation| &equation.residual)
        .chain(templates)
        .chain(bindings)
        .any(|expression| has_decidable_conditional(flat, expression))
}

fn has_decidable_conditional(flat: &flat::Model, expression: &Expression) -> bool {
    if let Expression::If { branches, .. } = expression
        && branches
            .iter()
            .any(|(condition, _)| reads_only_translation_values(flat, condition))
    {
        return true;
    }
    expression_children(expression)
        .into_iter()
        .any(|child| has_decidable_conditional(flat, child))
}

fn reads_only_translation_values(flat: &flat::Model, condition: &Expression) -> bool {
    let mut reads = ValueReads::default();
    rumoca_core::ExpressionVisitor::visit_expression(&mut reads, condition);
    reads.names.iter().all(|name| {
        flat.enum_literal_ordinals.contains_key(name.as_str())
            || flat.variables.get(name).is_some_and(|variable| {
                matches!(
                    variable.variability,
                    Variability::Parameter(_) | Variability::Constant(_)
                )
            })
    })
}

fn contains_conditional(expression: &Expression) -> bool {
    matches!(expression, Expression::If { .. })
        || expression_children(expression)
            .into_iter()
            .any(contains_conditional)
}

/// The selected rows, or `None` when no row changes.
fn selected_equations(
    selection: &mut Selection<'_>,
    equations: &[flat::Equation],
) -> Option<Vec<flat::Equation>> {
    let mut changed = false;
    let selected = equations
        .iter()
        .map(|equation| {
            let residual = selection.owned_by(equation.span, &equation.residual);
            let mut equation = equation.clone();
            if let Some(residual) = residual {
                changed = true;
                equation.residual = residual;
            }
            equation
        })
        .collect();
    changed.then_some(selected)
}

/// The selected declaration bindings that lower as equations: every binding
/// except a parameter's or constant's, which lowers as a value and keeps its
/// own attribute rule.
fn selected_bindings(
    selection: &mut Selection<'_>,
    flat: &flat::Model,
) -> Vec<(VarName, Expression)> {
    flat.variables
        .iter()
        .filter(|(_, variable)| {
            !matches!(
                variable.variability,
                Variability::Parameter(_) | Variability::Constant(_)
            )
        })
        .filter_map(|(name, variable)| {
            let binding = variable.binding.as_ref()?;
            let selected = selection.owned_by(variable.source_span, binding)?;
            Some((name.clone(), selected))
        })
        .collect()
}

/// The families with their selected compact bodies, or `None` when no body
/// changes.
fn selected_families(
    selection: &mut Selection<'_>,
    families: &[flat::StructuredEquationFamily],
) -> Option<Vec<flat::StructuredEquationFamily>> {
    let mut changed = false;
    let selected = families
        .iter()
        .map(|family| {
            let mut family = family.clone();
            if let Some(template) = selected_template(selection, &family) {
                changed = true;
                family.template = Some(template);
            }
            family
        })
        .collect();
    changed.then_some(selected)
}

fn selected_template(
    selection: &mut Selection<'_>,
    family: &flat::StructuredEquationFamily,
) -> Option<rumoca_core::ComprehensionTemplate> {
    let mut template = family.template.clone()?;
    let mut changed = false;
    for body in &mut template.body {
        if let Some(selected) = selection.owned_by(family.span, body) {
            changed = true;
            *body = selected;
        }
    }
    changed.then_some(template)
}

struct Selection<'a> {
    flat: &'a flat::Model,
    values: &'a ShapeEnvironment,
    evaluable: &'a HashSet<VarName>,
    decided: Vec<flat::ParameterBranchSelection>,
}

impl Selection<'_> {
    /// `expression` with its structural selections applied, or `None` when it
    /// holds none. `owner` is the equation each decision is recorded against.
    fn owned_by(&mut self, owner: Span, expression: &Expression) -> Option<Expression> {
        if !contains_conditional(expression) {
            return None;
        }
        let mut rewriter = SelectionRewriter {
            selection: self,
            owner,
            changed: false,
        };
        let selected = rewriter.rewrite_expression(expression);
        rewriter.changed.then_some(selected)
    }

    /// Whether the conditional at `span` is a structural selection: shape
    /// discovery could not certify its run-time arms, or its arms are not
    /// structurally equal under a tunable guard.
    fn selects(
        &self,
        branches: &[(Expression, Expression)],
        else_branch: &Expression,
        span: Span,
    ) -> bool {
        self.values.is_structural_selection(span)
            || !retains_flat_guard(self.flat, self.evaluable, branches, else_branch)
    }

    fn decided(&self, condition: &Expression) -> Option<bool> {
        match self.values.proven_value(condition) {
            Some(ProvenValue::Boolean(value)) => Some(value),
            _ => None,
        }
    }

    /// Record the ordinary parameters a decided condition reads.
    fn record(&mut self, owner: Span, condition: &Expression) {
        let references = ordinary_parameters(self.flat, self.evaluable, condition)
            .into_iter()
            .map(|name| vec![name.to_string()])
            .collect::<Vec<_>>();
        if references.is_empty() {
            return;
        }
        let selection = flat::ParameterBranchSelection {
            span: owner,
            kind: flat::StructuralParameterUse::BranchSelection,
            references,
        };
        if !self.decided.contains(&selection) {
            self.decided.push(selection);
        }
    }
}

struct SelectionRewriter<'s, 'a> {
    selection: &'s mut Selection<'a>,
    owner: Span,
    changed: bool,
}

impl ExpressionRewriter for SelectionRewriter<'_, '_> {
    fn rewrite_expression(&mut self, expression: &Expression) -> Expression {
        let Expression::If {
            branches,
            else_branch,
            span,
        } = expression
        else {
            return self.walk_expression(expression);
        };
        if !self.selection.selects(branches, else_branch, *span) {
            return self.walk_expression(expression);
        }
        self.select(branches, else_branch, *span)
    }
}

impl SelectionRewriter<'_, '_> {
    /// MLS §11.5: conditions are tested in order; a proven-false branch is
    /// never taken, a proven-true branch ends the chain, and an unproven
    /// condition keeps its arm.
    fn select(
        &mut self,
        branches: &[(Expression, Expression)],
        else_branch: &Expression,
        span: Span,
    ) -> Expression {
        let mut kept = Vec::with_capacity(branches.len());
        let mut taken = None;
        for (condition, value) in branches {
            let Some(decided) = self.selection.decided(condition) else {
                kept.push((
                    self.rewrite_expression(condition),
                    self.rewrite_expression(value),
                ));
                continue;
            };
            self.changed = true;
            self.selection.record(self.owner, condition);
            if decided {
                taken = Some(value);
                break;
            }
        }
        let fallback = self.rewrite_expression(taken.unwrap_or(else_branch));
        if kept.is_empty() {
            return fallback;
        }
        Expression::If {
            branches: kept,
            else_branch: Box::new(fallback),
            span,
        }
    }
}
