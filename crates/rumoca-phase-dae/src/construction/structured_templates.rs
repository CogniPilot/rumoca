//! Select optional symbolic views only from completed call-shape certificates.

mod effects;

use super::*;
use effects::{CallEffects, temporal};

pub(super) struct TemplateSelection {
    continuous: Vec<bool>,
    initialization: Vec<bool>,
}

impl TemplateSelection {
    #[cfg(test)]
    pub(super) fn all(flat: &flat::Model) -> Self {
        Self {
            continuous: vec![true; flat.structured_equations.len()],
            initialization: vec![true; flat.initial_structured_equations.len()],
        }
    }

    pub(super) fn analyze(flat: &flat::Model, shapes: &FunctionShapeAnalysis) -> Self {
        Self {
            continuous: select(flat, &flat.structured_equations, shapes),
            initialization: select(flat, &flat.initial_structured_equations, shapes),
        }
    }

    pub(super) fn continuous<'a>(&'a self, flat: &'a flat::Model) -> SelectedFamilies<'a> {
        SelectedFamilies {
            families: &flat.structured_equations,
            policy: TemplatePolicy::Selected(&self.continuous),
        }
    }

    pub(super) fn initialization<'a>(&'a self, flat: &'a flat::Model) -> SelectedFamilies<'a> {
        SelectedFamilies {
            families: &flat.initial_structured_equations,
            policy: TemplatePolicy::Selected(&self.initialization),
        }
    }
}

#[derive(Clone, Copy)]
enum TemplatePolicy<'a> {
    Selected(&'a [bool]),
    Mandatory,
}

#[derive(Clone, Copy)]
pub(super) struct SelectedFamilies<'a> {
    families: &'a [flat::StructuredEquationFamily],
    policy: TemplatePolicy<'a>,
}

impl<'a> SelectedFamilies<'a> {
    pub(super) fn mandatory(families: &'a [flat::StructuredEquationFamily]) -> Self {
        Self {
            families,
            policy: TemplatePolicy::Mandatory,
        }
    }

    pub(super) fn iter(self) -> impl Iterator<Item = SelectedFamily<'a>> {
        self.families
            .iter()
            .enumerate()
            .map(move |(ordinal, source)| {
                let enabled = match self.policy {
                    TemplatePolicy::Selected(selected) => selected[ordinal],
                    TemplatePolicy::Mandatory => !source.interiors_materialized,
                };
                SelectedFamily {
                    source,
                    template: source.template.as_ref().filter(|_| enabled),
                }
            })
    }

    pub(super) fn expressions(self) -> impl Iterator<Item = &'a Expression> {
        self.iter()
            .filter_map(|family| family.template)
            .flat_map(|template| &template.body)
    }
}

pub(super) struct SelectedFamily<'a> {
    source: &'a flat::StructuredEquationFamily,
    pub(super) template: Option<&'a rumoca_core::ComprehensionTemplate>,
}

impl<'a> SelectedFamily<'a> {
    /// The source template, which owns the family's discrete-value partition
    /// whether or not its compact view is selected.
    pub(super) fn source_template(&self) -> Option<&'a rumoca_core::ComprehensionTemplate> {
        self.source.template.as_ref()
    }
}

impl std::ops::Deref for SelectedFamily<'_> {
    type Target = flat::StructuredEquationFamily;

    fn deref(&self) -> &Self::Target {
        self.source
    }
}

fn select(
    flat: &flat::Model,
    families: &[flat::StructuredEquationFamily],
    shapes: &FunctionShapeAnalysis,
) -> Vec<bool> {
    let mut effects = CallEffects::default();
    families
        .iter()
        .map(|family| {
            if !family.interiors_materialized {
                return true;
            }
            let Some(template) = &family.template else {
                return false;
            };
            let Ok(scoped) =
                affine_slices::scoped_shapes(shapes.model_values(), &family.domain, family.span)
            else {
                return false;
            };
            template.body.iter().all(|body| {
                checked_calls(body, flat, shapes, &scoped, &mut effects)
                    && (!contains_call(body) || checked_residual_shapes(body, shapes, &scoped))
            })
        })
        .collect()
}

fn checked_calls(
    expression: &Expression,
    flat: &flat::Model,
    shapes: &FunctionShapeAnalysis,
    values: &ShapeEnvironment,
    effects: &mut CallEffects,
) -> bool {
    match expression {
        Expression::FunctionCall { name, .. }
            if shapes.expression_shape(expression, values).is_err()
                || !effects.permits(flat, name.var_name()) =>
        {
            return false;
        }
        Expression::BuiltinCall { function, .. } if temporal(*function) => return false,
        _ => {}
    }
    expression_children(expression)
        .into_iter()
        .all(|child| checked_calls(child, flat, shapes, values, effects))
}

fn contains_call(expression: &Expression) -> bool {
    matches!(expression, Expression::FunctionCall { .. })
        || expression_children(expression)
            .into_iter()
            .any(contains_call)
}

fn checked_residual_shapes(
    expression: &Expression,
    shapes: &FunctionShapeAnalysis,
    values: &ShapeEnvironment,
) -> bool {
    match expression {
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            branches.iter().all(|(condition, value)| {
                shapes
                    .expression_shape(condition, values)
                    .is_ok_and(|shape| shape.is_empty())
                    && checked_residual_shapes(value, shapes, values)
            }) && checked_residual_shapes(else_branch, shapes, values)
        }
        Expression::Binary {
            op: OpBinary::Sub,
            lhs,
            rhs,
            ..
        } => matches!(
            (shapes.expression_shape(lhs, values), shapes.expression_shape(rhs, values)),
            (Ok(left), Ok(right)) if left == right
        ),
        _ => shapes.expression_shape(expression, values).is_ok(),
    }
}
