//! Source-location-independent declaration identity used by semantic phases.

use crate::{ClassDef, Component, ComponentReference, Connection, Expression, Subscript, Visitor};
use rumoca_core::{Causality, DefId, Variability};
use std::ops::ControlFlow;

/// Return whether two components denote the same inherited declaration.
///
/// MLS §5.6 permits an element inherited through multiple paths to contribute
/// once. Distinct declaration IDs are compatible only when their complete
/// semantic declarations agree.
pub fn components_are_semantically_compatible(existing: &Component, incoming: &Component) -> bool {
    if let (Some(existing_def_id), Some(incoming_def_id)) = (existing.def_id, incoming.def_id)
        && existing_def_id == incoming_def_id
    {
        return true;
    }

    component_types_are_equivalent(existing, incoming)
        && component_prefixes_are_equivalent(existing, incoming)
        && component_values_are_equivalent(existing, incoming)
        && component_modifiers_are_equivalent(existing, incoming)
}

/// Return whether two inherited child classes have the same declaration.
pub fn classes_are_semantically_compatible(existing: &ClassDef, incoming: &ClassDef) -> bool {
    if let (Some(existing_def_id), Some(incoming_def_id)) = (existing.def_id, incoming.def_id)
        && existing_def_id == incoming_def_id
    {
        return true;
    }

    existing.to_modelica("") == incoming.to_modelica("")
}

fn component_types_are_equivalent(existing: &Component, incoming: &Component) -> bool {
    match (existing.type_def_id, incoming.type_def_id) {
        (Some(existing), Some(incoming)) => existing == incoming,
        _ => existing.type_name.to_string() == incoming.type_name.to_string(),
    }
}

fn component_prefixes_are_equivalent(existing: &Component, incoming: &Component) -> bool {
    variability_eq(&existing.variability, &incoming.variability)
        && causality_eq(&existing.causality, &incoming.causality)
        && connection_eq(&existing.connection, &incoming.connection)
        && existing.inner == incoming.inner
        && existing.outer == incoming.outer
        && existing.is_final == incoming.is_final
        && existing.is_replaceable == incoming.is_replaceable
        && existing.is_redeclare == incoming.is_redeclare
        && existing.constrainedby.as_ref().map(ToString::to_string)
            == incoming.constrainedby.as_ref().map(ToString::to_string)
}

fn variability_eq(a: &Variability, b: &Variability) -> bool {
    matches!(
        (a, b),
        (Variability::Empty, Variability::Empty)
            | (Variability::Constant(_), Variability::Constant(_))
            | (Variability::Discrete(_), Variability::Discrete(_))
            | (Variability::Parameter(_), Variability::Parameter(_))
            | (Variability::Continuous(_), Variability::Continuous(_))
    )
}

fn causality_eq(a: &Causality, b: &Causality) -> bool {
    matches!(
        (a, b),
        (Causality::Empty, Causality::Empty)
            | (Causality::Input(_), Causality::Input(_))
            | (Causality::Output(_), Causality::Output(_))
    )
}

fn connection_eq(a: &Connection, b: &Connection) -> bool {
    matches!(
        (a, b),
        (Connection::Empty, Connection::Empty)
            | (Connection::Flow(_), Connection::Flow(_))
            | (Connection::Stream(_), Connection::Stream(_))
    )
}

#[derive(Debug, Default, PartialEq)]
struct ExpressionIdentity {
    text: String,
    node_kinds: Vec<std::mem::Discriminant<Expression>>,
    references: Vec<(String, Option<DefId>)>,
}

impl Visitor for ExpressionIdentity {
    fn enter_expression(&mut self, expression: &Expression) -> ControlFlow<()> {
        self.node_kinds.push(std::mem::discriminant(expression));
        ControlFlow::Continue(())
    }

    fn visit_component_reference(&mut self, reference: &ComponentReference) -> ControlFlow<()> {
        self.references
            .push((reference.to_string(), reference.def_id));
        crate::walk_component_reference_default(self, reference)
    }

    fn visit_expr_function_call(
        &mut self,
        function: &ComponentReference,
        args: &[Expression],
    ) -> ControlFlow<()> {
        self.references
            .push((function.to_string(), function.def_id));
        crate::walk_component_reference_default(self, function)?;
        for argument in args {
            self.visit_expression(argument)?;
        }
        ControlFlow::Continue(())
    }
}

fn expressions_are_equivalent(existing: &Expression, incoming: &Expression) -> bool {
    expression_identity(existing) == expression_identity(incoming)
}

fn expression_identity(expression: &Expression) -> ExpressionIdentity {
    let mut identity = ExpressionIdentity {
        text: expression.to_string(),
        ..ExpressionIdentity::default()
    };
    let _ = identity.visit_expression(expression);
    identity
}

fn component_values_are_equivalent(existing: &Component, incoming: &Component) -> bool {
    existing.has_explicit_binding == incoming.has_explicit_binding
        && optional_expression_eq(existing.binding.as_ref(), incoming.binding.as_ref())
        && expressions_are_equivalent(&existing.start, &incoming.start)
        && existing.start_is_modification == incoming.start_is_modification
        && existing.start_has_each == incoming.start_has_each
        && existing.shape == incoming.shape
        && existing
            .shape_expr
            .iter()
            .zip(&incoming.shape_expr)
            .all(|(existing, incoming)| subscripts_are_equivalent(existing, incoming))
        && existing.shape_expr.len() == incoming.shape_expr.len()
        && existing.shape_is_modification == incoming.shape_is_modification
        && optional_expression_eq(existing.condition.as_ref(), incoming.condition.as_ref())
}

fn optional_expression_eq(existing: Option<&Expression>, incoming: Option<&Expression>) -> bool {
    match (existing, incoming) {
        (Some(existing), Some(incoming)) => expressions_are_equivalent(existing, incoming),
        (None, None) => true,
        _ => false,
    }
}

fn subscripts_are_equivalent(existing: &Subscript, incoming: &Subscript) -> bool {
    match (existing, incoming) {
        (Subscript::Empty, Subscript::Empty)
        | (Subscript::Range { .. }, Subscript::Range { .. }) => true,
        (Subscript::Expression(existing), Subscript::Expression(incoming)) => {
            expressions_are_equivalent(existing, incoming)
        }
        _ => false,
    }
}

fn component_modifiers_are_equivalent(existing: &Component, incoming: &Component) -> bool {
    let modifications_equal = existing.modifications.len() == incoming.modifications.len()
        && existing.modifications.iter().all(|(name, expression)| {
            incoming
                .modifications
                .get(name)
                .is_some_and(|incoming| expressions_are_equivalent(expression, incoming))
        });
    modifications_equal
        && existing.final_attributes == incoming.final_attributes
        && existing.each_modifications == incoming.each_modifications
}
